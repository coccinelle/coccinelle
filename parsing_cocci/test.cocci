// Copyright 2012-2015, Inria
// Julia Lawall, Gilles Muller
// Copyright 2010-2011, INRIA, University of Copenhagen
// Julia Lawall, Rene Rydhof Hansen, Gilles Muller, Nicolas Palix
// Copyright 2005-2009, Ecole des Mines de Nantes, University of Copenhagen
// Yoann Padioleau, Julia Lawall, Rene Rydhof Hansen, Henrik Stuart, Gilles Muller, Nicolas Palix
// This file is part of Coccinelle.
//
// Coccinelle is free software: you can redistribute it and/or modify
// it under the terms of the GNU General Public License as published by
// the Free Software Foundation, according to version 2 of the License.
//
// Coccinelle is distributed in the hope that it will be useful,
// but WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
// GNU General Public License for more details.
//
// You should have received a copy of the GNU General Public License
// along with Coccinelle.  If not, see <http://www.gnu.org/licenses/>.
//
// The authors reserve the right to distribute this or future versions of
// Coccinelle under other licenses.


@@
struct SHT sht;
local function proc_info_func;
@@
   sht.proc_info = proc_info_func;

@@
identifier buffer, start, offset, length, inout, hostptr, hostno;
@@
   proc_info_func (
+      struct Scsi_Host *hostptr,
       char *buffer, char **start, off_t offset, int length,
-      int hostno,
       int inout) {
    ...
-   struct Scsi_Host *hostptr;
    ...
-   hostptr = scsi_host_hn_get(hostno);
    ...
?-  if (!hostptr) { ... }
    ...
?-  scsi_host_put(hostptr);
    ...
  }

@@
expression E;
@@
  proc_info_func(...) {
    <...
(
\+-   E->host_no == hostno
+   E == shpnt
|
-   hostno
+   shpnt->host_no
)
    ...>
 }

@@
struct foo E;
@@
  proc_info_func(...) {
    <...
(
\+-   E->host_no == hostno
+   E == shpnt
|
-   hostno
+   shpnt->host_no
)
    ...>
 }
