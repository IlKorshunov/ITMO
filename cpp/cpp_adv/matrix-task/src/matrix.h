#pragma once

#include <algorithm>
#include <cassert>
#include <compare>
#include <cstddef>
#include <functional>
#include <iterator>
#include <numeric>
#include <utility>

template <class T>
class matrix {
private:
    template <class U>
    class basic_col_iterator {
        friend matrix;

    public:
        using value_type = T;
        using reference = U&;
        using pointer = U*;
        using difference_type = std::ptrdiff_t;
        using iterator_category = std::random_access_iterator_tag;

        basic_col_iterator() = default;

        basic_col_iterator(pointer data, size_t col_index, size_t col) : _data(data), _col_index(col_index), _col(col) {}

        operator basic_col_iterator<const U>() const {
            return basic_col_iterator<const U>(_data, _col_index, _col);
        }

        basic_col_iterator& operator++() {
            _data += _col;
            return *this;
        }

        basic_col_iterator& operator--() {
            _data -= _col;
            return *this;
        }

        basic_col_iterator operator++(int) {
            basic_col_iterator temp(*this);
            _data += _col;
            return temp;
        }

        basic_col_iterator operator--(int) {
            basic_col_iterator temp(*this);
            _data -= _col;
            return temp;
        }

        basic_col_iterator operator+(difference_type diff) const {
            basic_col_iterator temp(*this);
            temp._data += diff * _col;
            return temp;
        }

        friend basic_col_iterator operator+(difference_type diff, const basic_col_iterator& it) {
            return it + diff;
        }

        basic_col_iterator operator-(difference_type diff) const {
            basic_col_iterator temp(*this);
            temp._data -= diff * static_cast<difference_type>(_col);
            return temp;
        }

        friend difference_type operator-(const basic_col_iterator& left, const basic_col_iterator& right) {
            return (left._data - right._data) / static_cast<difference_type>(left._col);
        }

        basic_col_iterator& operator+=(difference_type diff) {
            _data += static_cast<difference_type>(_col) * diff;
            return *this;
        }

        basic_col_iterator& operator-=(difference_type diff) {
            _data -= static_cast<difference_type>(_col) * diff;
            return *this;
        }

        reference operator*() const {
            return _data[_col_index];
        }

        pointer operator->() const {
            return _data + _col_index;
        }

        reference operator[](difference_type offset) const {
            return *(_data + offset * static_cast<difference_type>(_col) + static_cast<difference_type>(_col_index));
        }

        friend auto operator<=>(const basic_col_iterator& left, const basic_col_iterator& right) {
            assert(left._col_index == right._col_index && left._col == right._col);
            return left._data <=> right._data;
        }

        friend bool operator==(const basic_col_iterator& left, const basic_col_iterator& right) {
            return std::is_eq(left <=> right);
        }

        friend bool operator!=(const basic_col_iterator& left, const basic_col_iterator& right) {
            return !std::is_eq(left <=> right);
        }

    private:
        pointer _data;
        size_t _col_index;
        size_t _col;
    };

public:
    using value_type = T;
    using reference = value_type&;
    using const_reference = const value_type&;
    using pointer = value_type*;
    using const_pointer = const value_type*;
    using iterator = pointer;
    using const_iterator = const_pointer;
    using row_iterator = pointer;
    using const_row_iterator = const_pointer;
    using col_iterator = basic_col_iterator<value_type>;
    using const_col_iterator = basic_col_iterator<const value_type>;

    matrix() : _data(nullptr), _row(0), _col(0) {}

    matrix(size_t rows, size_t cols) : _row(rows), _col(cols) {
        size_t cur_size = rows * cols;
        if (cur_size == 0) {
            _row = 0;
            _col = 0;
            _data = nullptr;
        } else {
            _data = new T[cur_size]{};
        }
    }

    template <size_t Rows, size_t Cols>
    matrix(const value_type (&init)[Rows][Cols]) : _data(new value_type[Rows * Cols]),
                                                   _row(Rows),
                                                   _col(Cols) {
        iterator start = begin();
        for (size_t i = 0; i < Rows; i++) {
            start = std::copy_n(init[i], Cols, start);
        }
    }

    matrix(const matrix& other)
        : _data(other.empty() ? nullptr : new T[other.size()]),
          _row(other._row),
          _col(other._col) {
        if (!other.empty()) {
            std::copy(other.begin(), other.end(), begin());
        } else {
            _row = 0;
            _col = 0;
        }
    }

    matrix(matrix&& other) : _data(other._data), _row(other._row), _col(other._col) {
        other._data = nullptr;
        other._row = 0;
        other._col = 0;
    }

    matrix& operator=(const matrix& other) {
        if (this == &other) {
            return *this;
        }
        matrix copy = matrix(other);
        swap(copy);
        return *this;
    }

    matrix& operator=(matrix&& other) {
        if (this == &other) {
            return *this;
        }
        delete[] _data;
        swap(other);
        other._data = nullptr;
        other._row = 0;
        other._col = 0;
        return *this;
    }

    ~matrix() {
        _row = 0;
        _col = 0;
        delete[] _data;
        _data = nullptr;
    }

    // Iterators
    iterator begin() {
        return data();
    }

    const_iterator begin() const {
        return data();
    }

    iterator end() {
        return data() + size();
    }

    const_iterator end() const {
        return data() + size();
    }

    row_iterator row_begin(size_t row) {
        assert(row < rows());
        return data() + row * cols();
    }

    const_row_iterator row_begin(size_t row) const {
        assert(row < rows());
        return data() + row * cols();
    }

    row_iterator row_end(size_t row) {
        assert(row < rows());
        return row_begin(row) + cols();
    }

    const_row_iterator row_end(size_t row) const {
        assert(row < rows());
        return row_begin(row) + cols();
    }

    col_iterator col_begin(size_t col) {
        assert(col < cols());
        return col_iterator(begin(), col, cols());
    }

    const_col_iterator col_begin(size_t col) const {
        assert(col < cols());
        return const_col_iterator(begin(), col, cols());
    }

    col_iterator col_end(size_t col) {
        assert(col < cols());
        return col_iterator(end(), col, cols());
    }

    const_col_iterator col_end(size_t col) const {
        assert(col < cols());
        return const_col_iterator(end(), col, cols());
    }

    // Size
    size_t rows() const {
        return _row;
    }

    size_t cols() const {
        return _col;
    }

    size_t size() const {
        return _row * _col;
    }

    bool empty() const {
        return _data == nullptr;
    }

    // Elements access
    reference operator()(size_t row, size_t col) {
        assert(row * _col + col < size() && "invalid input");
        return _data[row * _col + col];
    }

    const_reference operator()(size_t row, size_t col) const {
        assert(row * _col + col < size() && "invalid input");
        return _data[row * _col + col];
    }

    pointer data() {
        return _data;
    }

    const_pointer data() const {
        return _data;
    }

    // Comparison
    friend bool operator==(const matrix& left, const matrix& right) {
        if (left.cols() != right.cols() || left.rows() != right.rows()) {
            return false;
        }
        return std::equal(left.begin(), left.end(), right.begin());
    }

    friend bool operator!=(const matrix& left, const matrix& right) {
        return !(left == right);
    }

    // Arithmetic operations
    matrix& operator+=(const matrix& other) {
        return transformation(other, std::plus<value_type>());
    }

    matrix& operator-=(const matrix& other) {
        return transformation(other, std::minus<value_type>());
    }

    matrix& operator*=(const matrix& other) {
        return *this = multiply(other);
    }

    matrix& operator*=(const_reference factor) {
        return transformation(*this, [&factor](const_reference x, const_reference) { return x * factor; });
    }

    friend matrix operator+(const matrix& left, const matrix& right) {
        return matrix(left, right, std::plus<value_type>());
    }

    friend matrix operator-(const matrix& left, const matrix& right) {
        return matrix(left, right, std::minus<value_type>());
    }

    friend matrix operator*(const matrix& left, const matrix& right) {
        return left.multiply(right);
    }

    friend matrix operator*(const matrix& left, const_reference right) {
        return matrix(left, left, [&right](const_reference x, const_reference) { return x * right; });
    }

    friend matrix operator*(const_reference left, const matrix& right) {
        return operator*(right, left);
    }

private:
    pointer _data;
    size_t _row;
    size_t _col;

    template<class BinaryOperation>
    matrix(const matrix& left, const matrix& right, BinaryOperation operation)
        : _data(new value_type[left.size()]),
          _row(left.rows()),
          _col(left.cols()) {
        assert((left.cols() == right.cols() && left.rows() == right.rows()) && "incorrect dimension");
        std::transform(left.begin(), left.end(), right.begin(), _data, operation);
    }

    template<class BinaryOperation>
    matrix& transformation(const matrix& other, BinaryOperation operation) {
        assert((this->cols() == other.cols() && this->rows() == other.rows()) && "incorrect dimension");
        std::transform(this->begin(), this->end(), other.begin(), this->begin(), operation);
        return *this;
    }

    void swap(matrix& other) {
        std::swap(_col, other._col);
        std::swap(_row, other._row);
        std::swap(_data, other._data);
    }

    matrix multiply(const matrix& other) const {
        assert(cols() == other.rows());
        matrix result(rows(), other.cols());

        for (size_t i = 0; i < rows(); ++i) {
            for (size_t j = 0; j < other.cols(); ++j) {
                result(i, j) = value_type{};
                for (size_t k = 0; k < cols(); ++k) {
                    result(i, j) += (*this)(i, k) * other(k, j);
                }
            }
        }

        return result;
    }

};
