#include <vector>
#include <algorithm>
#include <cmath>
#include <iostream>
#include "so_vector.h"
#include <functional>
#include "big_integer.h"

typedef unsigned __int128 uint128_t;

static const uint64_t RADIX = 4294967296;
static const big_integer ZERO(0);

bool less_abs(big_integer const &a, big_integer const &b) {
    if (a.digits.size() < b.digits.size()) {
        return true;
    }
    if (a.digits.size() > b.digits.size()) {
        return false;
    }
    for(size_t i = a.digits.size() - 1; i + 1 > 0; i--) {
        if (a.digits[i] < b.digits[i]) {
            return true;
        }
        if (a.digits[i] > b.digits[i]) {
            return false;
        }
    }
    return false;
}

void big_integer::norm() {
    while(digits.size() > 1 && digits.back() == 0) {
        digits.pop_back();
    }
    if (digits.size() == 1 && digits[0] == 0) {
        sign = false;
    }
}

// переводит из бита под знак в дополнение до двух
void big_integer::bit_to_two() {
    if (!sign) {
        return;
    }
    for(uint32_t i = 0; i < digits.size(); i++) {
        digits[i] = ~digits[i];
    }
    sign = false;
    (*this)++;
}

// переводит из дополнения до двух в бит под знак
void big_integer::two_to_bit() {
    sign = false;
    if ((digits.back() & RADIX / 2) != RADIX / 2) {
        return;
    }
    (*this)--;
    for(uint32_t i = 0; i < digits.size(); i++) {
        digits[i] = ~digits[i];
    }
    sign = true;
}

// a = a/b, return a % b
uint32_t big_integer::div_little(uint32_t b) {
    uint64_t x = 0;
    for(size_t i = digits.size() - 1; i + 1 > 0; i--) {
        x = x * RADIX + digits[i];
        digits[i] = x / b;
        x %= b;
    }
    norm();
    return x;
}

void big_integer::mul_little(uint32_t b) {
    digits.push_back(0);
    uint64_t carry = 0;
    for(size_t i = 0; i < digits.size(); i++) {
        uint64_t x = static_cast<uint64_t>(digits[i]) * b + carry;
        digits[i] = x % RADIX;
        carry = x / RADIX;
    }
    norm();
}

uint128_t big_integer::get_prefix(big_integer const &a, size_t n) {
    uint128_t x = 0;
    for(size_t i = 0; i < n; i++) {
        x += a.digits[a.digits.size() - i - 1];
        x <<= 32;
    }
    x >>= 32;
    return x;
}

void big_integer::abs() {
    sign = false;
}

// a = a % bb, return a / bb
big_integer big_integer::div_(big_integer const &bb) {
    if (less_abs(*this, bb)) {
        return 0;
    }
    if (bb.digits.size() == 1) {
        big_integer rez((*this).div_little(bb.digits[0]));
        rez.sign = sign;
        sign ^= bb.sign;
        rez.norm();
        swap(rez);
        return rez;
    }
    big_integer b(bb);
    bool a_sign = sign;
    abs();
    b.abs();

    uint32_t f = RADIX / (b.digits.back() + 1);
    mul_little(f);
    b.mul_little(f);

    digits.push_back(0);
    uint128_t b2 = get_prefix(b, 2);
    big_integer rez;

    size_t prev_size = b.digits.size();
    b.digits.insert_begin(digits.size() - prev_size - 1, 0);

    big_integer c;
    for(size_t j = 0; j <= b.digits.size() - prev_size; j++) {

        uint128_t a3 = get_prefix(*this, 3);
        uint32_t d = a3 / b2;
        d = std::min(static_cast<uint32_t>(d), UINT32_MAX);

        c.digits.resize(b.digits.size() - j);
        for(size_t i = j; i < b.digits.size(); i++)
            c.digits[i - j] = b.digits[i];

        c.mul_little(d);
        norm();
        if (less_abs(*this, c)) {
            d--;
            c.sub_positive(b);
        }
        rez.digits.push_back(d);
        (*this).sub_positive(c);
        if (digits.size() < b.digits.size() - j) {
            digits.insert_end(b.digits.size() - digits.size() - j, 0);
        }
    }
    rez.sign = a_sign ^ bb.sign;
    rez.digits.reverse();
    rez.norm();
    sign = a_sign;
    div_little(f);
    return rez;
}

big_integer::big_integer()
        : sign(false) {
    digits.push_back(0);
}

big_integer::big_integer(big_integer const &other) = default;

big_integer::big_integer(int a) {
    sign = (a < 0);
    digits.push_back(std::abs(static_cast<int64_t>(a)));
}

big_integer::big_integer(uint32_t a) {
    sign = false;
    digits.push_back(a);
}

big_integer::big_integer(const std::string &str)
        : big_integer() {
    size_t st = 0;
    bool fl = false;
    if (str[0] == '-') {
        fl = true;
        st++;
    }
    if (str[0] == '+') {
        st++;
    }
    for(size_t i = st; i < str.size(); i++) {
        size_t j = i;
        uint32_t x = 0;
        uint32_t st10 = 1;
        const uint32_t NEW_RADIX = 10;
        while(j - i < 9 && j < str.size()) {
            x = x * NEW_RADIX + (str[j] - '0');
            j++;
            st10 *= NEW_RADIX;
        }
        i = j - 1;
        mul_little(st10);
        (*this) += x;
    }
    sign = fl;
    norm();
}

big_integer &big_integer::operator=(big_integer const &other) {
    big_integer res(other);
    swap(res);
    norm();
    return *this;
}

void big_integer::sub_positive(big_integer const &b) {
    int64_t borrow = 0;
    for(size_t i = 0; i < digits.size(); i++) {
        int64_t b_dig;
        (i < b.digits.size()) ? b_dig = b.digits[i] : b_dig = 0;
        if (static_cast<int64_t>(digits[i]) >= b_dig + borrow) {
            digits[i] = digits[i] - b_dig - borrow;
            borrow = 0;
        } else {
            uint64_t x = digits[i] + RADIX - b_dig - borrow;
            digits[i] = x;
            borrow = 1;
        }
    }
    norm();
}

void big_integer::add_positive(big_integer const &b) {
    digits.resize(std::max(b.digits.size(), digits.size()) + 1);
    int64_t carry = 0;
    for(size_t i = 0; i < digits.size(); i++) {
        int64_t b_dig;
        (i < b.digits.size()) ? b_dig = b.digits[i] : b_dig = 0;
        int64_t x = static_cast<int64_t>(digits[i]) + static_cast<int64_t>(b_dig) + carry;
        digits[i] = x % RADIX;
        carry = x / RADIX;
    }
    norm();
}

big_integer &big_integer::operator+=(big_integer const &rhs) {
    if (sign != rhs.sign) {
        (*this) -= -rhs;
    } else {
        add_positive(rhs);
    }
    return (*this);
}


big_integer &big_integer::operator-=(big_integer const &rhs) {
    if (sign == rhs.sign) {
        big_integer b(rhs);
        if (less_abs(*this, b)) {
            swap(b);
            sign = !sign;
        }
        int64_t borrow = 0;
        sub_positive(b);
    } else {
        (*this) += -rhs;
    }
    return (*this);
}

big_integer &big_integer::operator*=(big_integer const &rhs) {
    if (rhs == ZERO || *this == ZERO) {
        *this = ZERO;
        return *this;
    }
    big_integer a;
    a.digits.resize(this->digits.size() + rhs.digits.size() + 2);
    for(size_t i = 0; i < digits.size(); i++) {
        for(size_t j = 0; j < rhs.digits.size(); j++) {
            uint64_t x = rhs.digits[j];
            x *= digits[i];
            uint64_t y = x % RADIX + a.digits[i + j];
            a.digits[i + j] = y % RADIX;
            uint64_t z = x / RADIX + y / RADIX + a.digits[i + j + 1];
            a.digits[i + j + 1] = z % RADIX;
            a.digits[i + j + 2] += z / RADIX;
        }
    }
    a.sign = sign ^ rhs.sign;
    a.norm();
    swap(a);
    return *this;
}

big_integer &big_integer::operator/=(big_integer const &rhs) {
    auto rez = (*this).div_(rhs);
    swap(rez);
    return *this;
}

big_integer &big_integer::operator%=(big_integer const &rhs) {
    div_(rhs);
    return *this;
}

big_integer &big_integer::bitwise(big_integer const &rhs, const std::function<uint32_t(uint32_t, uint32_t)> &f) {
    big_integer b(rhs);
    size_t mx = std::max(digits.size(), b.digits.size()) + 1;
    digits.resize(mx);
    b.digits.resize(mx);
    bit_to_two();
    b.bit_to_two();
    for(size_t i = 0; i < mx; i++) {
        digits[i] = f(digits[i], b.digits[i]);
    }
    two_to_bit();
    norm();
    return *this;
}

big_integer &big_integer::operator&=(big_integer const &rhs) {
    return bitwise(rhs, [](uint32_t a, uint32_t b) { return a & b; });
}

big_integer &big_integer::operator|=(big_integer const &rhs) {
    return bitwise(rhs, [](uint32_t a, uint32_t b) { return a | b; });
}

big_integer &big_integer::operator^=(big_integer const &rhs) {
    return bitwise(rhs, [](uint32_t a, uint32_t b) { return a ^ b; });
}

// mul
big_integer &big_integer::operator<<=(int rhs) {
    const int POW2 = 32;
    int kl = rhs / POW2;
    rhs %= POW2;
    digits.insert_begin(kl, 0);
    uint32_t e = 1;
    e <<= rhs;
    return *this *= e;
}

// div
big_integer &big_integer::operator>>=(int rhs) {
    const int POW2 = 32;
    int kl = rhs / POW2;
    rhs %= POW2;
    digits.erase_begin(kl);
    uint32_t e = 1;
    e <<= rhs;
    (*this) /= e;
    if (sign && (*this) % e != ZERO) {
        (*this)--;
    }
    return *this;
}

big_integer big_integer::operator+() const {
    return *this;
}

big_integer big_integer::operator-() const {
    big_integer a(*this);
    a.sign = !a.sign;
    a.norm();
    return a;
}

big_integer big_integer::operator~() const {
    return -(*this) - 1;
}

big_integer &big_integer::operator++() {
    return *this += 1;
}

big_integer big_integer::operator++(int) {
    big_integer a(*this);
    *this += 1;
    return a;
}

big_integer &big_integer::operator--() {
    return *this -= 1;
}

big_integer big_integer::operator--(int) {
    big_integer a(*this);
    *this -= 1;
    return a;
}

std::string to_string(big_integer const &a) {
    if (a == ZERO) {
        return "0";
    }
    std::string s;
    big_integer b(a);
    b.abs();
    const uint32_t ST10_9 = 1000000000;
    while(b != ZERO) {
        uint32_t rem = b.div_little(ST10_9);
        std::string y = std::to_string(rem);
        reverse(y.begin(), y.end());
        y.insert(y.end(), 9 - y.size(), '0');
        s += y;
    }
    while(s.size() != 1 && s.back() == '0') {
        s.pop_back();
    }
    reverse(s.begin(), s.end());
    if (a.sign) {
        s = '-' + s;
    }
    return s;
}

void big_integer::swap(big_integer &b) {
    using std::swap;
    swap(sign, b.sign);
    digits.swap(b.digits);
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

int flags(big_integer const &a, big_integer const &b) {
    // '<' = 1;  '>' = -1; '==' 0;
    if (a.sign && !b.sign) {
        return 1;
    }
    if (b.sign && !a.sign) {
        return -1;
    }
    int ans = 0;
    if (a.digits.size() < b.digits.size()) {
        ans = 1;
    } else if (a.digits.size() > b.digits.size()) {
        ans = -1;
    } else {
        for(size_t i = a.digits.size() - 1; i + 1 > 0; i--) {
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
    if (ans == 0) {
        return ans;
    }
    if (a.sign && b.sign) {
        return -ans;
    }
    return ans;
}

bool operator<(big_integer const &a, big_integer const &b) {
    int f = flags(a, b);
    return (f == 1);
}

bool operator==(big_integer const &a, big_integer const &b) {
    return flags(a, b) == 0;
}

bool operator>(big_integer const &a, big_integer const &b) {
    int f = flags(a, b);
    return (f == -1);
}

bool operator!=(big_integer const &a, big_integer const &b) {
    return flags(a, b) != 0;
}

bool operator<=(big_integer const &a, big_integer const &b) {
    int f = flags(a, b);
    return (f >= 0);
}

bool operator>=(big_integer const &a, big_integer const &b) {
    int f = flags(a, b);
    return (f <= 0);
}

std::ostream &operator<<(std::ostream &cout_, big_integer const &a) {
    return cout_ << to_string(a);
}
