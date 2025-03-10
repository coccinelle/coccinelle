#define I830FALLBACK(s, arg...)                               \
do {                                                  \
    if (I830PTR(pScrn)->fallback_debug) {             \
      xf86DrvMsg(pScrn->scrnIndex, X_INFO,            \
                 "EXA fallback: " s "\n", ##arg);     \
    }                                                 \
    return FALSE;                                     \
} while(0)


int main () {
  I830FALLBACK(a,b);
}
