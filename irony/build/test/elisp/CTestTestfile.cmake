# CMake generated Testfile for 
# Source directory: /home/chenchunsheng/.emacs.d/elpa/irony-20141013.1005/server/test/elisp
# Build directory: /home/chenchunsheng/.emacs.d/irony/build/test/elisp
# 
# This file includes the relevent testing commands required for 
# testing this directory and lists subdirectories to be tested as well.
ADD_TEST(irony-el "/usr/bin/emacs" "-batch" "-l" "package" "--eval" "(package-initialize) (unless (require 'cl-lib nil t) (package-refresh-contents) (package-install 'cl-lib))" "-l" "/home/chenchunsheng/.emacs.d/elpa/irony-20141013.1005/server/test/elisp/irony.el" "-f" "ert-run-tests-batch-and-exit")
ADD_TEST(irony-cdb-el "/usr/bin/emacs" "-batch" "-l" "package" "--eval" "(package-initialize) (unless (require 'cl-lib nil t) (package-refresh-contents) (package-install 'cl-lib))" "-l" "/home/chenchunsheng/.emacs.d/elpa/irony-20141013.1005/server/test/elisp/irony-cdb.el" "-f" "ert-run-tests-batch-and-exit")
