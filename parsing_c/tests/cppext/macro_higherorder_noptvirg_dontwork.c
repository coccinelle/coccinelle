//parse error 
// = File "tests/rule81/w9968cf.c", line 781, column 1,  charpos = 34589
//    around = 'for', whole content = 	for (i = 0; i < cam->nbuffers; i++) {

void main(int i)
{
	if (cam->nbuffers != cam->max_buffers)
		DBG(2, "Couldn't allocate memory for %u video frame buffers. "
		       "Only memory for %u buffers has been allocated",
		    cam->max_buffers, cam->nbuffers)

	for (i = 0; i < cam->nbuffers; i++) {
		cam->frame[i].buffer = buff + i*vpp_bufsize;

        }
}
