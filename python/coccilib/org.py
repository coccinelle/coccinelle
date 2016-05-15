def build_link(p, msg, color) :
        return "[[view:%s::face=%s::linb=%s::colb=%s::cole=%s][%s]]" % (p.file,color,p.line,p.column,p.column_end,msg)

def print_todo(p, msg="", color="ovl-face1") :
        if msg == "" : msg = "%s::%s" % (p.file,p.line)
        link = build_link(p, msg, color)
        print ("* TODO %s" % (link))

def print_link(p, msg="", color="ovl-face1") :
        if msg == "" : msg = "%s::%s" % (p.file,p.line)
        print (build_link(p, msg, color))

def print_safe_todo(p, msg="", color="ovl-face1") :
        msg_safe=msg.replace("[","@(").replace("]",")")
        print_todo(p, msg_safe, color)

def print_safe_link(p, msg="", color="ovl-face1") :
        msg_safe=msg.replace("[","@(").replace("]",")")
        print_link(p, msg_safe, color)

#
# print_main, print_sec and print_secs
#
def print_main(msg, p, color="ovl-face1") :
        if msg == "" :
                oldmsgfmt = "%s::%s" % (p[0].file,p[0].line)
        else:
                oldmsgfmt = "%s %s::%s" % (msg,p[0].file,p[0].line)
        print_todo(p[0], oldmsgfmt, color)

def print_sec(msg, p, color="ovl-face2") :
        print_link(p[0], msg, color)

def print_secs(msg, ps, color="ovl-face2") :
        for i in ps:
                print_link (i, msg, color)
