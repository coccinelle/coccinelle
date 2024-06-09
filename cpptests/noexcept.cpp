void foo() noexcept {}

void Logger::trace(LogLocation loc, int mess) noexcept
{ }

void Logger::trace(LogLocation loc, std::string_view mess) noexcept
{ }

formatter_type create_formatter(boost::log::attribute_name const &name, args_map const &args) override
{ return NULL; }
