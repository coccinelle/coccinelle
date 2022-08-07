class CpuList {
 public:

  // Returns true if `this` and `other` have identical bitmaps, false
  // otherwise.
  // Returns the Union of `this` and `other`.
  int operator+=(const CpuList& other) {
    this->Union(other);
    return *this;
  }

  // Returns the Union of `lhs` and `rhs`.
  CpuList operator+(CpuList lhs, const CpuList& rhs) {
    lhs += rhs;
    return lhs;
  }

 private:
  // Returns the result of `this.Subtract(other)`.
  CpuList operator-=(const CpuList& other) {
    this->Subtract(other);
    return *this;
  }

  // Returns the result of `lhs.Subtract(rhs)`.
  CpuList operator-(CpuList lhs, const CpuList& rhs) {
    lhs -= rhs;
    return lhs;
  }
};
