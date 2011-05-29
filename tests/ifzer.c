int main() {
#if 0 /* Accessing floppy->pc is not valid here, the previous pc may be gone
          and have lived on another thread's stack; that stack may have become
          unmapped meanwhile (CONFIG_DEBUG_PAGEALLOC). */
float4 cTest::test(float2 vec)
{
}
#endif
 foo();
}
 
