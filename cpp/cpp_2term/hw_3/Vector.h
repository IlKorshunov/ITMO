#pragma once
#include "MyException.h"
#include "return_codes.h"

#include <cstdint>
#include <cstdlib>
#include <cstring>
#include <iosfwd>

class Vector
{
  private:
	uint32_t* _data = nullptr;
	size_t _size = 0;
	size_t _capacity = 0;

  public:
	Vector();
	Vector(const Vector& other);
	Vector(const Vector& other, size_t new_capacity);
	~Vector();

	void swap(Vector& other);

	Vector& operator=(const Vector& other);

	size_t size() const { return _size; }

	bool empty() const { return _size == 0; }

	void pop_back() { --_size; }

	void clear() { _size = 0; }

	void push_back(uint32_t val)
	{
		if (_size + 1 > _capacity)
		{
			Vector(*this, _capacity == 0 ? 1 : _capacity * 2).swap(*this);
		}
		_data[_size++] = val;
	}

	void resize(size_t new_size, uint32_t val = 0)
	{
		while (_size < new_size)
		{
			push_back(val);
		}
	}

	uint32_t& operator[](size_t index) { return _data[index]; }

	const uint32_t& operator[](size_t index) const { return _data[index]; }

	uint32_t& at(size_t index)
	{
		if (index > _size || index < 0)
		{
			throw MyException(ERROR_UNKNOWN, "Invalid index");
		}
		return _data[index];
	}

	const uint32_t& at(size_t index) const
	{
		if (index > _size || index < 0)
		{
			throw MyException(ERROR_UNKNOWN, "Invalid index");
		}
		return _data[index];
	}

	const uint32_t& back() const
	{
		if (_size == 0)
		{
			throw MyException(ERROR_UNKNOWN, "vector is empty");
		}
		return _data[size() - 1];
	}

	uint32_t* begin() { return _data; }

	const uint32_t* begin() const { return _data; }
};
