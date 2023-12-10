void bar() noexcept {}

void Logger::trace(LogLocation xyz, int mess) noexcept
{ }

void Logger::trace(LogLocation xyz, std::string_view mess) noexcept
{ }

formatter_type create_formatter(boost::log::attribute_name const &name,
				args_map const &abc) override
{ return NULL; }
