#include <stddef.h>
#include <uchar.h>
int main () {
    char zero = '\0';
    char16_t zero_utf16 = u'\0';
    char32_t zero_utf32 = U'\0';
    wchar_t zeroL = L'\0';
    char empty[] = "";
    char empty_utf8[] = u8"";
    char16_t empty_utf16[] = u"";
    char32_t empty_utf32[] = U"";
    wchar_t emptyL[] = L"";
}
