
#standard Python code
import logging,sys
print "(In Python code: log.py)"

formatter = logging.Formatter('%(name)s :%(asctime)s %(filename)s %(lineno)s %(levelname)s  %(message)s')
stdout_handler = logging.StreamHandler(sys.stdout)
stdout_handler.setFormatter(formatter)
logger=logging.getLogger('')
logger.addHandler(stdout_handler)
logger.setLevel(logging.DEBUG)

def create_ocamllog( logger):
    def ocamllog(level,msg):
        """logging module will try to walk up the python stack (using sys._getframe) to
        find details about the layer that called the logging module.
        This fails for ocaml as there is no such information.
        One can get around this by setting logging._srcfile to none (temporarily).
        See /usr/lib/python2.3/logging/__init__.py

        A hack (not thread safe I suppose). fangohr 3/9/2006"""
        logging._srcfile_org = logging._srcfile
        logging._srcfile = None
        logger.log(level,msg)
        logging._srcfile = logging._srcfile_org
    return ocamllog
    
#now register logger with ocaml
logger=logging.getLogger('nmesh-ocaml')
ocaml.register_logger('nmesh',create_ocamllog(logger))
logger=logging.getLogger('nfem-ocaml')
ocaml.register_logger('nfem',create_ocamllog(logger))
logger=logging.getLogger('root-ocaml')
ocaml.register_logger('root',create_ocamllog(logger))


ocaml.log("ocaml",30,'Message from ocaml logger sent from Python with level 30')

#now call this logger from ocaml(!)
if 1 == 1:

    ocaml.log("ocaml",30,'Message from ocaml with level 30')
    logger.setLevel(35)
    ocaml.log("default",30,'Message from ocaml with level 30')
    ocaml.log("default",40,'Message from ocaml with level 40')

print "Done (python)"

