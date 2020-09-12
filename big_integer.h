#ifndef BIG_INTEGER_H
#define BIG_INTEGER_H

#include <cstddef>
#include <string>
#include "so_vector.h"
#include <functional>

typedef unsigned __int128 uint128_t;

struct big_integer {
private:

    so_vector digits;
    bool sign;

public:
    big_integer();                                          // ok
    big_integer(big_integer const &other);                  // ok
    big_integer(int a);                                     // ok
    big_integer(uint32_t a);                            // just another one constructor ^^
    explicit big_integer(std::string const &str);           // ok

    big_integer &operator=(big_integer const &other);       // ok

    big_integer &operator+=(big_integer const &rhs);        // ok
    big_integer &operator-=(big_integer const &rhs);        // ok
    big_integer &operator*=(big_integer const &rhs);        // ok
    big_integer &operator/=(big_integer const &rhs);        // ok
    big_integer &operator%=(big_integer const &rhs);        // ok

    big_integer &operator&=(big_integer const &rhs);        // ok
    big_integer &operator|=(big_integer const &rhs);        // ok
    big_integer &operator^=(big_integer const &rhs);        // ok

    big_integer &operator<<=(int rhs);                      // ok
    big_integer &operator>>=(int rhs);                      // ok

    big_integer operator+() const;                          // ok
    big_integer operator-() const;                          // ok
    big_integer operator~() const;                          // ok

    big_integer &operator++();                              // ok
    big_integer operator++(int);                            // ok

    big_integer &operator--();                              // ok
    big_integer operator--(int);                            // ok

    friend std::string to_string(big_integer const &a);     // ok

private:
    friend int flags(big_integer const &a, big_integer const &b);
    friend bool less_abs(big_integer const &a, big_integer const &b);
    void norm();

    void sub_positive(big_integer const &b);
    void add_positive(big_integer const &b);

    void bit_to_two();
    void two_to_bit();

    uint32_t div_little(uint32_t b);
    void mul_little(uint32_t b);
    uint128_t get_prefix(big_integer const &a, size_t n);
    void abs();
    big_integer div_(big_integer const &b);
    void swap(big_integer &b);
    big_integer &bitwise(big_integer const &rhs, const std::function<uint32_t(uint32_t, uint32_t)> &f);
};

big_integer operator+(big_integer a, big_integer const &b);     // ok
big_integer operator-(big_integer a, big_integer const &b);     // ok
big_integer operator*(big_integer a, big_integer const &b);     // ok
big_integer operator/(big_integer a, big_integer const &b);     // ok
big_integer operator%(big_integer a, big_integer const &b);     // ok

big_integer operator&(big_integer a, big_integer const &b);     // ok
big_integer operator|(big_integer a, big_integer const &b);     // ok
big_integer operator^(big_integer a, big_integer const &b);     // ok

big_integer operator<<(big_integer a, int b);                   // ok
big_integer operator>>(big_integer a, int b);                   // ok

bool operator==(big_integer const &a, big_integer const &b);    // ok
bool operator!=(big_integer const &a, big_integer const &b);    // ok
bool operator<(big_integer const &a, big_integer const &b);     // ok
bool operator>(big_integer const &a, big_integer const &b);     // ok
bool operator<=(big_integer const &a, big_integer const &b);    // ok
bool operator>=(big_integer const &a, big_integer const &b);    // ok

std::ostream &operator<<(std::ostream &s, big_integer const &a);

#endif // BIG_INTEGER_H