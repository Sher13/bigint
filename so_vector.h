#pragma once


#include <algorithm>
#include <vector>
#include "cow_vector.h"
#include <memory>

size_t const SIZE = 6;

struct so_vector {
private:
    bool is_small;
    size_t size_sm;
    union {
        uint32_t small[SIZE]{};
        cow_vector* big;
    };

public:

    so_vector()
            : is_small(true), size_sm(0) {};

    so_vector(so_vector const &b)
            : is_small(b.is_small), size_sm(b.size_sm) {
        if (is_small) {
            for(size_t i = 0; i < size_sm; i++) {
                small[i] = b.small[i];
            }
        } else {
            b.big->inc();
            big = b.big;
        }
    }

    so_vector &operator=(so_vector const &other) {
        so_vector res(other);
        swap(res);
        return *this;
    }

    ~so_vector() {
        if (!is_small) {
            big->dec();
            if (big->is_zero()) {
                delete big;
            }
        }
    }

    size_t size() const {
        if (is_small) {
            return size_sm;
        }
        return take().size();
    }

    uint32_t &operator[](size_t x) {
        if (is_small) {
            return small[x];
        }
        return take()[x];
    };

    uint32_t const &operator[](size_t x) const {
        if (is_small) {
            return small[x];
        }
        return take()[x];
    };

    uint32_t &back() {
        if (is_small) {
            return small[size_sm - 1];
        }
        return take().back();
    };

    uint32_t const &back() const {
        if (is_small) {
            return small[size_sm - 1];
        }
        return take().back();
    };

    void pop_back() {
        if (is_small) {
            size_sm--;
            return;
        }
        take().pop_back();
        if (take().size() == SIZE) {
            big_to_small();
        }
    }

    void push_back(const uint32_t &val) {
        if (is_small && size_sm < SIZE) {
            small[size_sm] = val;
            size_sm++;
            return;
        }
        if (is_small) {
            small_to_big();
        }
        take().push_back(val);
    }

    void insert_begin(size_t n, const uint32_t &val) {
        size_t now_size = size();
        for(size_t i = 0; i < n; i++) {
            push_back(val);
        }
        for(size_t i = now_size - 1; i + 1 > 0; i--) {
            std::swap((*this)[i], (*this)[i + n]);
        }
    }

    void insert_end(size_t n, const uint32_t &val) {
        size_t now_size = size();
        for(size_t i = 0; i < n; i++) {
            push_back(val);
        }
    }

    void erase_begin(size_t n) {
        if (is_small) {
            for(size_t i = 0; i < n; i++)
                small[i] = small[i + n];
            return;
        }
        take().erase(take().begin(), take().begin() + n);
        if (take().size() <= SIZE) {
            big_to_small();
        }
    }

    void resize(size_t n) {
        if (n == size())
            return;
        if (n > size()) {
            while(size() != n) {
                push_back(0);
            }
            return;
        }
        if (n < size()) {
            while(size() != n) {
                pop_back();
            }
            return;
        }
    }

    void reverse() {
        if (is_small) {
            for(size_t i = 0; i < size_sm / 2; i++) {
                std::swap(small[i], small[size_sm - i - 1]);
            }
            return;
        }
        std::reverse(take().begin(), take().end());
    }

    void swap(so_vector &b) {
        std::swap(is_small, b.is_small);
        std::swap(small, b.small);
        std::swap(size_sm, b.size_sm);
        //std::swap(big, b.big);
    }

private:
    void small_to_big() {
        uint32_t copy[SIZE];
        for(size_t i = 0; i < SIZE; i++) {
            copy[i] = small[i];
        }
        big = new cow_vector(SIZE);
        for(size_t i = 0; i < SIZE; i++) {
            take()[i] = copy[i];
        }
        is_small = false;
    };
    void big_to_small() {
        std::vector<uint32_t> copy(take());
        for(size_t i = 0; i < SIZE; i++) {
            copy[i] = take()[i];
        }
        big->dec();
        if (big->is_zero()) {
            delete big;
        }
        for(size_t i = 0; i < SIZE; i++) {
            small[i] = copy[i];
        }
        is_small = true;

    }

    std::vector<uint32_t>& take() {
        if (big->is_one()) {
            return big->data();
        }
        big->dec();
        big = new cow_vector(big->data());
        return big->data();
    }

    std::vector<uint32_t>& take() const {
        return big->data();
    }
};
