struct foo {
        size_t element_count;
        int element_array[] __counted_by(element_count);
};
