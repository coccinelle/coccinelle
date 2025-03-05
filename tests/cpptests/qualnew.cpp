struct ValueUnion {
  explicit ValueUnion(size_t BuffSize)
      : Size(sizeof(DataT) + BuffSize),
        Buff(::new (std::malloc(Size)) DataT(), &std::free) {}
};
