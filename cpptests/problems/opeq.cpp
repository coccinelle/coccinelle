inline bool operator==(const batch<T, A>& lhs, const std::array<T, N>& rhs)
     {
         std::array<T, N> tmp;
         lhs.store_unaligned(tmp.data());
         return tmp == rhs;
     }
