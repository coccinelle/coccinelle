int main() {
	if (uffdio_zeropage.zeropage == -EEXIST) {
		fprintf(stderr, "UFFDIO_ZEROPAGE -EEXIST\n");
		exit(1);
	}
	else {
		fprintf(stderr, "UFFDIO_ZEROPAGE error %Ld\n");
		exit(1);
	}
}

int main() {
	if (uffdio_zeropage.zeropage == -EEXIST)
		fprintf(stderr, "UFFDIO_ZEROPAGE -EEXIST\n");
	else {
		fprintf(stderr, "UFFDIO_ZEROPAGE error %Ld\n");
		exit(1);
	}
}
