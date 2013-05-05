# Coccinelle output module to display Firehose XML

try:
    from firehose.model import Message, Function, Point, Range, \
    File, Location, Generator, Metadata, Analysis, Issue, Notes, Failure, \
    CustomFields
except ImportError:
    print "Error: Firehose is not installed or not in the Python path."
    print "Try 'pip install firehose' as root."
    import sys; sys.exit()


def __build_analysis(sut = None, file_ = None, stats = None,
                     gen_name="coccinelle", gen_version=""):
    """Creates a new Analysis() object.
    
    Keyword arguments:
    sut -- software under test
    file_ --
    stats --
    gen_name -- the generator name, default 'coccinelle'
    gen_version -- the generator version
    """

    generator = Generator(name=gen_name,
                          version=gen_version)
    metadata = Metadata(generator, sut, file_, stats)
    analysis = Analysis(metadata, [])
    
    return analysis

def __build_issue(location, message,
                  cwe=None, testid=None, notes=None,
                  trace=None, severity=None, customfields=None):
    """Creates a new Issue() object.
    
    Arguments:
    location -- a coccinelle position (cocci.elems.Location)
    message --
    
    Keyword arguments:
    cwe -- 
    testid --
    notes --
    trace --
    severity --
    customfields --
    
    """
    location = __coccilocation_to_firehoselocation(location[0])
    message = Message(message)
    
    issue = Issue(cwe, testid, location, message, notes, trace,
                  severity=severity, customfields=customfields)
    
    return issue

def __coccilocation_to_firehoselocation(cocciloc):
    """Converts a Coccinelle Location object to a Firehose one.
    The range attribute is used (single point not yet supported).
    
    Arguments:
    cocciloc -- coccilib.elems.Location instance
    
    """
    file_ = File(cocciloc.file, "")
    function = Function(cocciloc.current_element)
    range_ = Range(Point(int(cocciloc.line), int(cocciloc.column)),
                   Point(int(cocciloc.line_end), int(cocciloc.column_end))
                   )
    
    return Location(file_, function, range_=range_)

def print_issue(location, message):
    """Displays the Firehose XML output of an issue.
    This function is intended to be called from a .cocci file.
    
    Arguments:
    location -- a coccinelle position (cocci.elems.Location)
    message --
    
    """
    analysis = __build_analysis()
    issue = __build_issue(location, message)
    analysis.results.append(issue)
    
    print(str(analysis.to_xml_bytes()))
