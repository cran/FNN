
12/12/2007

Bugs and limitations in knnFinder (based on ANN 0.2):

(1) Memory leakage.
(2) Maximum distance is small (ANN_DIST_INF	=  999999.99);
(3) Query data is only the RIGHT MOST COLUMN OF A DATA FRAME OR MATRIX (nn function).


RANN 2.1 package is the replacement of knnFinder by the same authors using ANN 1.1.2.
nn2 function is added with more interface to the search type and tree type.
SELF_MATCH is allowed.


1/8/2008 Develop FNN_0.1
7/9/2008 version 0.2 uses namespace.

Note:
(1) Folder sr_complete contains cover tree, ANN and related KNN source files. ANN library is prebuilt on Windows platform. g++ with either CygWin or MinGW works fine. 	
 Linux will need build library from this folder.
(2) SELF_MATCH is disabled for ANN.
set ANN_ALLOW_SELF_MATCH = ANNfalse in include/ANN/ANN.h file.


3/15/2010 test on Windows XP SP3, Pentium D 3GHz, 3GB of RAM, R 2.10.1:
On a PC: kd tree is fast for low-dimensional data, p<10, e.g. p=5.
VR is fast for medium dimension (20 < p < 30).
cover tree is fast for lager high-dimensional data (>60).

On a Mac:

All three are similar for 40<p<60.
cover tree and kd tree are  fast for lager high-dimensional data (>60), but for larger n, kd tree is slow.


