# Coccinelle output module to display Firehose XML

"""
You need Firehose to use this module.
You can get it at https://github.com/fedora-static-analysis/firehose
or directly via Pypi: # pip install firehose

In your semantic patches, import Firehose like this:
coccilib.xml_firehose.import_firehose()
"""

def import_firehose():
    """
    inserts Firehose module into globals()
    
    It is required to proceed this way, as long as we can't use the
    classical approach (from firehose.model import Foo[...]), mainly
    because this module is imported by coccinelle wether or not it
    will be used by the semantic patch (and we don't want to display
    an error message to non-Firehose users.
    """
    try:
        globals()['firehose'] = __import__('firehose.model')
    except ImportError:
        print "Error: Firehose is not installed or not in the Python path."

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

    generator = firehose.model.Generator(name=gen_name,
                                         version=gen_version)
    metadata = firehose.model.Metadata(generator, sut, file_, stats)
    analysis = firehose.model.Analysis(metadata, [])
    
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
    message = firehose.model.Message(message)
    
    issue = firehose.model.Issue(cwe, testid, location, message, notes, trace,
                                 severity=severity, customfields=customfields)
    
    return issue

def __coccilocation_to_firehoselocation(cocciloc):
    """Converts a Coccinelle Location object to a Firehose one.
    The range attribute is used (single point not yet supported).
    
    Arguments:
    cocciloc -- coccilib.elems.Location instance
    
    """
    file_ = firehose.model.File(cocciloc.file, "")
    function = firehose.model.Function(cocciloc.current_element)
    range_ = firehose.model.Range(firehose.model.Point(int(cocciloc.line),
                                                       int(cocciloc.column)),
                                  firehose.model.Point(int(cocciloc.line_end),
                                                       int(cocciloc.column_end))
                                  )
    
    return firehose.model.Location(file_, function, range_=range_)

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
