def print_main(msg, p, color="ovl-face1") :
	print "* TODO [[view:%s::face=%s::linb=%s::colb=%s::cole=%s][%s %s::%s]]" % (p[0].file,color,p[0].line,p[0].column,p[0].column_end,msg,p[0].file,p[0].line)

def print_sec(msg, p, color="ovl-face2") :
	print "[[view:%s::face=%s::linb=%s::colb=%s::cole=%s][%s]]" % (p[0].file,color,p[0].line,p[0].column,p[0].column_end,msg)

def print_secs(msg, ps, color="ovl-face2") :
	for i in ps:
		print "[[view:%s::face=%s::linb=%s::colb=%s::cole=%s][%s]]" % (i.file,color,i.line,i.column,i.column_end,msg)
