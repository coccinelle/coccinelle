void main (int i) 
{
	DBG(5, "Video post-processing module detected")

out:

        PDBGG("Isochrnous frame: length %u, #%u i", len, i)

		/*
		   NOTE: It is probably correct to assume that SOF and EOF
		         headers do not occur between two consecutive packets,
		         but who knows..Whatever is the truth, this assumption
		         doesn't introduce bugs.
		*/

redo:


	DBG(3, "Video device /dev/video%d is open", cam->v4ldev->minor)

out:
        foo();

}
