struct example_struct_t : public base_type_t {
    example_struct_t() : base_type_t()
        { }

    void do_something() {
        REDTRC(0, 0, 0, "Testing %s %d", member1.c_str(), member2);
    }

    std::string member1 = {"test"};
    int member2 = {42};
} global_instance1;
