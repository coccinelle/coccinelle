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
    because this module is imported by coccinelle whether or not it
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

def print_issue(location=None,
                message=None,
                sut_type=None,
                sut_name=None,
                sut_version=None,
                sut_buildarch=None):
    """Displays the Firehose XML output of an issue.
    This function is intended to be called from a .cocci file.
    
    Arguments:
    location -- a coccinelle position (cocci.elems.Location)
    message --
    
    """
    
    def _extract_version_release(long_version):
        """
        extracts the version and the release from long_version
        # e.g. 1.5-2-4.2 -> 1.5-2 -- 4.2
        """
        sep = long_version.rfind("-")
        version = long_version[:sep]
        release = long_version[sep+1:]
        return version, release
    
    #import_firehose()
    if sut_type == "source-rpm":
        sut_release = "" # TODO for RPM
        sut = firehose.model.SourceRpm(sut_name, sut_version,
                                       sut_release, sut_buildarch)
    elif sut_type == "debian-binary":
        sut_version, sut_release = _extract_version_release(sut_version)
        sut = firehose.model.DebianBinary(sut_name, sut_version,
                                          sut_release, sut_buildarch)
    elif sut_type == "debian-source":
        sut_version, sut_release = _extract_version_release(sut_version)
        sut = firehose.model.DebianSource(sut_name, sut_version, sut_release)
    else:
        sut = None
    
    analysis = __build_analysis(sut=sut)
    issue = __build_issue(location, message)
    analysis.results.append(issue)
    
    print(str(analysis.to_xml_bytes()))
