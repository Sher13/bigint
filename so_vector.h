#pragma once


#include <algorithm>
#include <vector>
#include "cow_vector.h"
#include <memory>

struct so_vector {
private:
    static constexpr size_t SIZE = 6;
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
            std::copy(b.small, b.small + b.size_sm, small);
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
        if (is_small && size_sm + n <= SIZE) {
            std::fill(small+size_sm, small+size_sm+n, val);
            std::rotate(small, small+size_sm, small + size_sm + n);
            size_sm += n;
            return;
        }
        if (is_small) {
            small_to_big();
        }
        take().insert(take().begin(), n, val);
    }

    void insert_end(size_t n, const uint32_t &val) {
        if (is_small && size_sm + n <= SIZE) {
            std::fill(small+size_sm, small+size_sm+n, val);
            size_sm += n;
            return;
        }
        if (is_small) {
            small_to_big();
        }
        take().insert(take().end(), n, val);
    }

    void erase_begin(size_t n) {
        if (is_small) {
            std::copy(small+n, small+size_sm, small);
            return;
        }
        take().erase(take().begin(), take().begin() + n);
    }

    void resize(size_t n) {
        if (n == size()) {
            return;
        }
        if (n > size()) {
            insert_end(n - size(), 0);
        }
        if (n < size()) {
            if (is_small) {
                size_sm = n;
                return;
            }
            take().resize(n);
        }
    }

    void reverse() {
        if (is_small) {
            std::reverse(small, small + size_sm);
            return;
        }
        std::reverse(take().begin(), take().end());
    }

    void swap(so_vector &b) {
        std::swap(is_small, b.is_small);
        std::swap(small, b.small);
        std::swap(size_sm, b.size_sm);
    }

private:
    void small_to_big() {
        big = new cow_vector(std::vector<uint32_t>(small, small + size_sm));
        is_small = false;
    };

    std::vector<uint32_t> &take() {
        if (big->is_one()) {
            return big->data_;
        }
        big->dec();
        big = new cow_vector(big->data_);
        return big->data_;
    }

    std::vector<uint32_t> &take() const {
        return big->data_;
    }
};
