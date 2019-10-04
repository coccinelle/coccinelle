@@
typedef char16_t, char32_t, wchar_t;
identifier zero;
@@
(
- char zero = '\0';
|
- char16_t zero = u'\0';
|
- char32_t zero = U'\0';
|
- wchar_t zero = L'\0';
)


@@
identifier empty;
@@
(
- char empty[] = "";
|
- char empty[] = u8"";
|
- char16_t empty[] = u"";
|
- char32_t empty[] = U"";
|
- wchar_t empty[] = L"";
)
