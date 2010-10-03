int main() {
	if (rep.nEvents) {
		if (! (tc = (XTimeCoord *)
			Xmalloc( (unsigned)
				(nbytes = (long) rep.nEvents * sizeof(XTimeCoord))))) {
		  return;
		}
	}
}
