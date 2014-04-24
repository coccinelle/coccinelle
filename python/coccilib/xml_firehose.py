# Coccinelle output module to display Firehose XML

import sys

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
        print("Error: Firehose is not installed or not in the Python path.")

class Analysis(object):
    """
    An Analysis object wraps the work to keep tracks of results,
    and print the Firehose Analysis at the end.
    """
    
    def __init__(self,
                 use_env_variables=True,
                 sut_type=None,
                 sut_name=None,
                 sut_version=None,
                 sut_buildarch=None,
                 generator_version="",
                 root_path="",
                 blank_if_no_results=False):
        """
        Initializes an analysis and sets its useful variables
        
        Arguments:
        use_env_variables: if set to True, looks for the variables in bash
           environment, otherwise it uses the below parameters
           Do for example before calling spatch:
           export COCCI_SUT_TYPE="debian-source"
           export COCCI_SUT_NAME="package name"
           export COCCI_SUT_VERSION="package version"
           export COCCI_SUT_BUILARCH="pakage buildarch"
           export COCCI_GENERATOR_VERSION="1.0"
           export COCCI_ROOT_PATH="/path/to/project/"
           export COCCI_BLANK_IF_NO_RESULTS=1
           
        sut_type: "debian-source", "debian-binary" or "source-rpm"
        sut_name: the name of the package or software being analysed
        sut_version: its version
        sut_builarch: its architecture (only for "debian-binary", "source-rpm")
        generator_version: the version of Coccinelle
        root_path: the prefix to remove from the files paths
                   e.g. file = /path/to/project/path/to/file
                        root_path = /path/to/project/
                        file becomes path/to/file
        blank_if_no_results: defaults to False. If set to True, doesn't
            print anything if the analysis has 0 result.
        """
        if use_env_variables:
            import os
            
            self.sut_type = os.environ.get("COCCI_SUT_TYPE") or ""
            self.sut_name = os.environ.get("COCCI_SUT_NAME") or ""
            self.sut_version = os.environ.get("COCCI_SUT_VERSION") or ""
            self.sut_buildarch = os.environ.get("COCCI_SUT_BUILDARCH") or ""
            self.generator_version = os.environ.get(
                "COCCI_GENERATOR_VERSION") or ""
            self.root_path = os.environ.get("COCCI_ROOT_PATH") or ""
            self.blank_if_no_results = os.environ.get(
                "COCCI_BLANK_IF_NO_RESULTS") or False
        else:
            self.sut_type = sut_type
            self.sut_name = sut_name
            self.sut_version = sut_version
            self.sut_buildarch = sut_buildarch
            self.generator_version = generator_version
            self.root_path = root_path
            self.blank_if_no_results = blank_if_no_results
        
        self.results = []
        
    def add_result(self, location, message, cwe=None, testid=None, notes=None,
                   severity=None, customfields=None):
        """
        Adds a result in the analysis
        
        Arguments:
        location: a coccinelle position (cocci.elems.Location)
        message: the message to display with
        """
        self.results.append(self.__build_issue(location, message, cwe=cwe,
                                               testid=testid, notes=notes,
                                               severity=severity,
                                               customfields=customfields))
        

    def __build_analysis(self,
                         sut = None, file_ = None, stats = None,
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

    def __build_issue(self,
                      location, message,
                      cwe=None, testid=None, notes=None,
                      severity=None, customfields=None):
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
        location = self.__coccilocation_to_firehoselocation(location[0])
        message = firehose.model.Message(message)
        
        if notes is not None:
            notes = firehose.model.Notes(notes)
        
        if customfields is not None:
            customfields = firehose.model.CustomFields(customfields)
        
        # TODO: trace -> how to pass a trace from spatch to xml_firehose
        trace = None
        
        issue = firehose.model.Issue(cwe, testid, location, message, notes,
                                     trace, severity=severity,
                                     customfields=customfields)
        
        return issue

    def __coccilocation_to_firehoselocation(self, cocciloc):
        """Converts a Coccinelle Location object to a Firehose one.
        The range attribute is used (single point not yet supported).
        
        Arguments:
        cocciloc -- coccilib.elems.Location instance
        
        """
        # removes root_path from the file name
        filename = cocciloc.file
        if filename.startswith(self.root_path):
            filename = filename[len(self.root_path):]
        # removes the slash at begin of the filename if there's one:
        if filename.startswith("/"):
            filename = filename[1:]
        
        file_ = firehose.model.File(filename, "")
        function = firehose.model.Function(cocciloc.current_element)
        range_ = firehose.model.Range(
            firehose.model.Point(int(cocciloc.line),
                                 int(cocciloc.column)),
            firehose.model.Point(int(cocciloc.line_end),
                                 int(cocciloc.column_end))
            )
    
        return firehose.model.Location(file_, function, range_=range_)
    
    def print_analysis(self):
        """Displays the Firehose XML output of an issue.
        This function is intended to be called from a .cocci file.
        """
    
        def _extract_version_release(long_version):
            """
            extracts the version and the release from long_version
            # e.g. 1.5-2-4.2 -> 1.5-2 -- 4.2
            """
            sep = long_version.rfind("-")
            if sep == -1:
                version = long_version
                release = ""
            else:
                version = long_version[:sep]
                release = long_version[sep+1:]
            return version, release
        
        # if the analysis has 0 result and blank_if_no_results is True,
        # we don't print anything
        if not(self.results) and self.blank_if_no_results:
            sys.stdout.write("\n")
            return
            
        if self.sut_type == "source-rpm":
            self.sut_release = "" # TODO for RPM
            sut = firehose.model.SourceRpm(self.sut_name, self.sut_version,
                                           self.sut_release, self.sut_buildarch)
        elif self.sut_type == "debian-binary":
            self.sut_version, self.sut_release = _extract_version_release(
                self.sut_version)
            
            sut = firehose.model.DebianBinary(
                self.sut_name, self.sut_version,
                self.sut_release, self.sut_buildarch)
            
        elif self.sut_type == "debian-source":
            self.sut_version, self.sut_release = _extract_version_release(
                self.sut_version)
            sut = firehose.model.DebianSource(
                self.sut_name, self.sut_version, self.sut_release)
            
        else:
            sut = None
            sys.stderr.write("Warning: sut_type incorrect, should be "
                             "debian-source, debian-binary or source-rpm\n")
            
        analysis = self.__build_analysis(sut=sut,
                                         gen_version=self.generator_version)
        analysis.results = self.results
    
        sys.stdout.write(str(analysis.to_xml_bytes()))
        sys.stdout.write("\n")
