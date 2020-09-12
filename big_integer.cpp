#include <vector>
#include <iostream>
#include <algorithm>
#include "big_integer.h"

typedef unsigned __int128 uint128_t;

int flags(big_integer const &a, big_integer const &b) {
    // '<' = 1;  '>' = -1; '==' 0;
    if (a.sign && !b.sign)
        return 1;
    if (b.sign && !a.sign)
        return -1;
    int ans = 0;
    if (a.digits.size() < b.digits.size()) {
        ans = 1;
    } else if (a.digits.size() > b.digits.size()) {
        ans = -1;
    } else {
        for(int i = a.digits.size() - 1; i >= 0; i--) {
            if (a.digits[i] < b.digits[i]) {
                ans = 1;
                break;
            }
            if (a.digits[i] > b.digits[i]) {
                ans = -1;
                break;
            }
        }
    }
    if (ans == 0)
        return ans;
    if (a.sign && b.sign)
        return 0 - ans;
    return ans;
}

void big_integer::norm() {
    while(this->digits.size() && this->digits[this->digits.size() - 1] == 0)
        this->digits.pop_back();
    if (this->digits.size() == 0)
        this->sign = false;
}

big_integer big_integer::abs_(big_integer a) {
    a.sign = false;
    return a;
}

// переводит из бита под знак в дополнение до двух
void big_integer::bitToTwo() {
    if (!sign)
        return;
    for(size_t i = 0; i < digits.size(); i++) {
        digits[i] = ~digits[i];
    }
    sign = false;
    (*this)++;
}

// переводит из дополнения до двух в бит под знак
void big_integer::twoToBit() {
    if ((digits.back() & 2147483648) != 2147483648)
        return;
    (*this)--;
    for(size_t i = 0; i < digits.size(); i++) {
        digits[i] = ~digits[i];
    }
    sign = true;
}


std::pair<big_integer, big_integer> big_integer::div_little(big_integer a, big_integer b) {
    unsigned int b1 = b.digits[0];
    unsigned long long x = 0;
    big_integer rez;
    reverse(a.digits.begin(), a.digits.end());
    for(size_t i = 0; i < a.digits.size() - 1; i++) {
        x = x * radix + a.digits[i];
        rez.digits.push_back(x / b1);
        x %= b1;
    }
    x = x * radix + a.digits.back();
    if (x >= b1) {
        rez.digits.push_back(x / b1);
        x %= b1;
    }
    if (a.sign != b.sign) {
        rez.sign = true;
    }
    reverse(rez.digits.begin(), rez.digits.end());
    rez.norm();
    unsigned int x1 = x;
    return std::make_pair(rez, big_integer(x1));
}

std::pair<big_integer, big_integer> big_integer::div_(big_integer a, big_integer b) {
    //  a < b
    if (flags(a, b) == 1) {
        big_integer q;
        return std::make_pair(q, a);
    }
    if (b.digits.size() == 1) {
        return div_little(a, b); // I'm write it later....
    }
    a.digits.push_back(0);
    reverse(a.digits.begin(), a.digits.end());
    reverse(b.digits.begin(), b.digits.end());
    int f = radix / (a.digits[0] + 1);
    a *= f;
    b *= f;
    big_integer rez;
    bool fl = true;
    while(a.digits.size() >= b.digits.size()) {
        uint128_t a3 = a.digits[0] * radix * radix + a.digits[1] * radix + a.digits[2];
        uint128_t b2 = b.digits[0] * radix + b.digits[1];
        unsigned int d = a3 / b2;
        unsigned int d1 = std::min(d, UINT32_MAX);
        big_integer c = b * d1;
        if (flags(a, c) == 1) {
            d1--;
            c -= b;
        }
        if (d1 == 0)
            fl = false;
        rez.digits.push_back(d1);
        a -= c;
        if (fl) {
            a.digits.insert(a.digits.begin(), 0);
        }
    }
    if (a.sign != b.sign) {
        rez.sign = true;
    }
    reverse(rez.digits.begin(), rez.digits.end());
    rez.norm();
    big_integer x = rez * b;
    x = a - x;
    x = div_little(x, f).first;
    return std::make_pair(rez, x);
}

big_integer::big_integer() {
    sign = false;
};

big_integer::big_integer(big_integer const &other) {
    for(size_t i = 0; i < other.digits.size(); i++)
        digits.push_back(other.digits[i]);
    sign = other.sign;
};

big_integer::big_integer(int a) {
    sign = false;
    if (a < 0)
        sign = true;
    digits.push_back(abs(a));
};

big_integer::big_integer(unsigned int a) {
    sign = false;
    digits.push_back(a);
};

big_integer::big_integer(const std::string &str)
{
    big_integer a;
    int st = 0;
    if (str[0] == '-') {
        a.sign = true;
        st++;
    }
    for (size_t i = st; i < str.size(); i++) {
        a *= 10;
        a += (str[i]-'0');
    }
    *this = a;
}

big_integer &big_integer::operator=(big_integer const &other) {
    big_integer res = big_integer(other);
    this->digits = res.digits;
    this->sign = res.sign;
    return *this;
};

big_integer &big_integer::operator+=(big_integer const &rhs) {
    this->digits.resize(std::max(this->digits.size(), rhs.digits.size()) + 1);
    big_integer a;
    a.digits.resize(this->digits.size());
    big_integer b(rhs);
    b.digits.resize(this->digits.size());
    if (this->sign != rhs.sign) {
        if (abs_(*this) < abs_(b)) {
            big_integer c(*this);
            *this = b;
            b = c;
        }
        a.sign = this->sign;
        for(size_t i = 0; i < a.digits.size() - 1; i++) {
            if (this->digits[i] >= b.digits[i])
                a.digits[i] += this->digits[i] - b.digits[i];
            else {
                long long x = radix - b.digits[i] + this->digits[i];
                this->digits[i + 1]--;
                a.digits[i] = x;
            }
        }
        a.norm();
        *this = a;
        return *this;
    } else {
        a.digits.resize(a.digits.size() + 1);
        for(size_t i = 0; i < this->digits.size(); i++) {
            long long c = (long long) this->digits[i] + (long long) b.digits[i];
            a.digits[i] += c % radix;
            a.digits[i + 1] = c / radix;
        }
        a.norm();
        *this = a;
        return *this;
    }
};


big_integer &big_integer::operator-=(big_integer const &rhs) {
    return *this += (-rhs);
};

big_integer &big_integer::operator*=(big_integer const &rhs) {
    if (rhs.digits.empty()) {
        *this = rhs;
        return *this;
    }
    big_integer a;
    a.digits.resize(this->digits.size() + rhs.digits.size() + 2);
    if (this->sign != rhs.sign) {
        a.sign = true;
    } else {
        a.sign = false;
    }
    for(size_t i = 0; i < this->digits.size(); i++) {
        for(size_t j = 0; j < rhs.digits.size(); j++) {
            unsigned long long x = (unsigned long long) (this->digits[i]) * (unsigned long long) (rhs.digits[j]);
            unsigned long long mx = radix;
            unsigned long long z = (x % mx + (unsigned long long) a.digits[i + j]);
            a.digits[i + j] = z % mx;
            unsigned long long y = z / mx + x / mx;
            a.digits[i + j + 1] += y % mx;
            a.digits[i + j + 2] += y / mx;
        }
    }
    a.norm();
    *this = a;
    return *this;
};

big_integer &big_integer::operator/=(big_integer const &rhs) {
    big_integer a(*this);
    auto rez = div_(a, rhs);
    *this = rez.first;
    return *this;
}

big_integer &big_integer::operator%=(big_integer const &rhs) {
    big_integer a(*this);
    auto rez = div_(a, rhs);
    *this = rez.second;
    return *this;
}

big_integer &big_integer::operator&=(big_integer const &rhs) {
    big_integer a(*this);
    big_integer b(rhs);
    b.bitToTwo();
    a.bitToTwo();
    big_integer c;
    c.digits.resize(std::min(a.digits.size(), b.digits.size()));
    for(size_t i = 0; i < c.digits.size(); i++) {
        c.digits[i] = a.digits[i] & b.digits[i];
    }
    c.twoToBit();
    *this = c;
    return *this;
}

big_integer &big_integer::operator|=(big_integer const &rhs) {
    big_integer a(*this);
    a = ~a;
    a &= ~rhs;
    *this = ~a;
    return *this;
}

big_integer &big_integer::operator^=(big_integer const &rhs) {
    big_integer a(*this);
    a = ~a;
    a &= rhs;
    big_integer b(*this);
    b &= ~rhs;
    a |= b;
    *this = a;
    return *this;
}

// mul
big_integer &big_integer::operator<<=(int rhs) {
    int kl = rhs / 32;
    rhs %= 32;
    digits.insert(digits.begin(), kl, 0);
    unsigned int e = 1;
    e <<= rhs;
    return *this *= e;
}

// div
big_integer &big_integer::operator>>=(int rhs) {
    int kl = rhs / 32;
    rhs %= 32;
    digits.erase(digits.begin(), digits.begin() + kl);
    unsigned int e = 1;
    e <<= rhs;
    return *this /= e;
}

big_integer big_integer::operator+() const {
    return *this;
};

big_integer big_integer::operator-() const {
    big_integer a(*this);
    a.sign = !a.sign;
    a.norm();
    return a;
};

big_integer big_integer::operator~() const {
    big_integer a(*this);
    a.bitToTwo();
    for(size_t i = 0; i < a.digits.size(); i++) {
        a.digits[i] = ~a.digits[i];
    }
    a.twoToBit();
    return a;
}

big_integer &big_integer::operator++() {
    return *this += 1;
};

big_integer big_integer::operator++(int) {
    return *this += 1;
};

big_integer &big_integer::operator--() {
    return *this -= 1;
};

big_integer big_integer::operator--(int) {
    return *this -= 1;
};

std::string to_string(big_integer const& a) {
    std::string s;
    big_integer b(a);
    while(b.digits.size() != 0) {
        big_integer x = b % 10;
        s += (char) (x.digits[0]+'0');
        b /= 10;
    }
    while(s.back() == '0') {
        s.erase(s.end()-1);
    }
    reverse(s.begin(), s.end());
    return s;
}

big_integer operator+(big_integer a, big_integer const &b) {
    return a += b;
}

big_integer operator-(big_integer a, big_integer const &b) {
    return a -= b;
}

big_integer operator*(big_integer a, big_integer const &b) {
    return a *= b;
}

big_integer operator/(big_integer a, big_integer const &b) {
    return a /= b;
}

big_integer operator%(big_integer a, big_integer const &b) {
    return a %= b;
}

big_integer operator&(big_integer a, big_integer const &b) {
    return a &= b;
}

big_integer operator|(big_integer a, big_integer const &b) {
    return a |= b;
}

big_integer operator^(big_integer a, big_integer const &b) {
    return a ^= b;
}

big_integer operator<<(big_integer a, int b) {
    return a <<= b;
}

big_integer operator>>(big_integer a, int b) {
    return a >>= b;
}

bool operator<(big_integer const &a, big_integer const &b) {
    int f = flags(a, b);
    return (f == 1);
}

bool operator==(big_integer const &a, big_integer const &b) {
    int f = flags(a, b);
    return (f == 0);
}

bool operator>(big_integer const &a, big_integer const &b) {
    int f = flags(a, b);
    return (f == -1);
}

bool operator!=(big_integer const &a, big_integer const &b) {
    int f = flags(a, b);
    return !(f == 0);
};

bool operator<=(big_integer const &a, big_integer const &b) {
    int f = flags(a, b);
    return (f >= 0);
};

bool operator>=(big_integer const &a, big_integer const &b) {
    int f = flags(a, b);
    return (f <= 0);
}

std::ostream& operator<<(std::ostream& cout_, big_integer const& a) {
    cout_ << to_string(a);
    return cout_;
}

int main() {
    big_integer a("123452351498340918391834");
    a++;
    std::cout << a;
    return 0;
}

