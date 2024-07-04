# [mdb](https://github.com/TomMelt/mdb)

## Prerequisite

Python >= 3.10

## Installation

Clone the repository
```
git clone https://github.com/TomMelt/mdb.git
cd mdb
```

Create a virtual Python virtual environment
```
python3 -m venv $HOME/mdb-venv
```

Activate the virtual environement
```
source $HOME/mdb-venv/bin/activate
```

Install mdb and the optional dependency with pip3
```
pip3 install .
pip3 install termgraph
```

## Notes

As part of the CONQUEST project. I (Connor Aird) tried using mdb on the UCL cluster, Myriad. I installed mdb following the above guide within an interactive session with the following modules loaded.
```
Currently Loaded Modulefiles:
 1) beta-modules      3) compilers/intel/2022.2     5) libxc/6.2.2/intel-2022   7) openssl/1.1.1u   9) gerun                 11) emacs/28.1
 2) gcc-libs/10.2.0   4) mpi/intel/2021.6.0/intel   6) cmake/3.21.1             8) python/3.11.4   10) git/2.41.0-lfs-3.3.0  12) userscripts/1.4.0
``` 

I then launched an mdb sessions in the background using 
```
mdb launch -b gdb -n 4 -t ./Conquest
Ctrl+z
bg
```
Then, attached to the session, which appeared to be successful.
```
(mdb-venv) [ccaecai@node-d00a-124 np-4-debug]$ mdb attach
mdb - mpi debugger - built on various backends. Type ? for more info. To exit interactive mode type "q", "quit", "Ctrl+D" or "Ctrl+]".
(mdb 0-3) 
```
Adding a breakpoint seems to be successful,
```
(mdb 0-3) command break exx_kernel_default.f90:204
0:	Breakpoint 2 at 0x7ee299: file exx_kernel_default.f90, line 204.
************************************************************************
1:	Breakpoint 2 at 0x7ee299: file exx_kernel_default.f90, line 204.
************************************************************************
2:	Breakpoint 2 at 0x7ee299: file exx_kernel_default.f90, line 204.
************************************************************************
3:	Breakpoint 2 at 0x7ee299: file exx_kernel_default.f90, line 204.
```
However, trying to run until hitting this breakpoint results in an error
```
(mdb 0-3) command c
0:	Continuing.
0:	[cli_0]: write_line error; fd=9 buf=:cmd=init pmi_version=1 pmi_subversion=1
0:	:
0:	system msg for write_line failure : Bad file descriptor
0:	[cli_0]: Unable to write to PMI_fd
0:	[cli_0]: write_line error; fd=9 buf=:cmd=get_appnum
0:	:
0:	system msg for write_line failure : Bad file descriptor
0:	Abort(1090831) on node 0 (rank 0 in comm 0): Fatal error in PMPI_Init: Other MPI error, error stack:
0:	MPIR_Init_thread(178):
0:	MPID_Init(1427)......:
0:	MPIR_pmi_init(129)...: PMI_Get_appnum returned -1
0:	[cli_0]: write_line error; fd=9 buf=:cmd=abort exitcode=1090831
0:	:
0:	system msg for write_line failure : Bad file descriptor
0:
0:	Program received signal SIGSEGV, Segmentation fault.
0:	MPIR_Err_return_comm (comm_ptr=0x7fffffff1100, fcname=0x0, errcode=1090831) at ../../src/mpi/errhan/errutil.c:309
0:	309	../../src/mpi/errhan/errutil.c: No such file or directory.
0:	Missing separate debuginfos, use: debuginfo-install numactl-libs-2.0.12-5.el7.x86_64
************************************************************************
1:	Continuing.
1:	[cli_1]: write_line error; fd=10 buf=:cmd=init pmi_version=1 pmi_subversion=1
1:	:
1:	system msg for write_line failure : Bad file descriptor
1:	[cli_1]: Unable to write to PMI_fd
1:	[cli_1]: write_line error; fd=10 buf=:cmd=get_appnum
1:	:
1:	system msg for write_line failure : Bad file descriptor
1:	Abort(1090831) on node 0 (rank 0 in comm 0): Fatal error in PMPI_Init: Other MPI error, error stack:
1:	MPIR_Init_thread(178):
1:	MPID_Init(1427)......:
1:	MPIR_pmi_init(129)...: PMI_Get_appnum returned -1
1:	[cli_1]: write_line error; fd=10 buf=:cmd=abort exitcode=1090831
1:	:
1:	system msg for write_line failure : Bad file descriptor
1:
1:	Program received signal SIGSEGV, Segmentation fault.
1:	MPIR_Err_return_comm (comm_ptr=0x7fffffff1100, fcname=0x0, errcode=1090831) at ../../src/mpi/errhan/errutil.c:309
1:	309	../../src/mpi/errhan/errutil.c: No such file or directory.
1:	Missing separate debuginfos, use: debuginfo-install numactl-libs-2.0.12-5.el7.x86_64
************************************************************************
2:	Continuing.
2:	[cli_2]: write_line error; fd=15 buf=:cmd=init pmi_version=1 pmi_subversion=1
2:	:
2:	system msg for write_line failure : Bad file descriptor
2:	[cli_2]: Unable to write to PMI_fd
2:	[cli_2]: write_line error; fd=15 buf=:cmd=get_appnum
2:	:
2:	system msg for write_line failure : Bad file descriptor
2:	Abort(1090831) on node 0 (rank 0 in comm 0): Fatal error in PMPI_Init: Other MPI error, error stack:
2:	MPIR_Init_thread(178):
2:	MPID_Init(1427)......:
2:	MPIR_pmi_init(129)...: PMI_Get_appnum returned -1
2:	[cli_2]: write_line error; fd=15 buf=:cmd=abort exitcode=1090831
2:	:
2:	system msg for write_line failure : Bad file descriptor
2:
2:	Program received signal SIGSEGV, Segmentation fault.
2:	MPIR_Err_return_comm (comm_ptr=0x7fffffff1100, fcname=0x0, errcode=1090831) at ../../src/mpi/errhan/errutil.c:309
2:	309	../../src/mpi/errhan/errutil.c: No such file or directory.
2:	Missing separate debuginfos, use: debuginfo-install numactl-libs-2.0.12-5.el7.x86_64
************************************************************************
3:	Continuing.
3:	[cli_3]: write_line error; fd=19 buf=:cmd=init pmi_version=1 pmi_subversion=1
3:	:
3:	system msg for write_line failure : Bad file descriptor
3:	[cli_3]: Unable to write to PMI_fd
3:	[cli_3]: write_line error; fd=19 buf=:cmd=get_appnum
3:	:
3:	system msg for write_line failure : Bad file descriptor
3:	Abort(1090831) on node 0 (rank 0 in comm 0): Fatal error in PMPI_Init: Other MPI error, error stack:
3:	MPIR_Init_thread(178):
3:	MPID_Init(1427)......:
3:	MPIR_pmi_init(129)...: PMI_Get_appnum returned -1
3:	[cli_3]: write_line error; fd=19 buf=:cmd=abort exitcode=1090831
3:	:
3:	system msg for write_line failure : Bad file descriptor
3:
3:	Program received signal SIGSEGV, Segmentation fault.
3:	MPIR_Err_return_comm (comm_ptr=0x7fffffff1100, fcname=0x0, errcode=1090831) at ../../src/mpi/errhan/errutil.c:309
3:	309	../../src/mpi/errhan/errutil.c: No such file or directory.
3:	Missing separate debuginfos, use: debuginfo-install numactl-libs-2.0.12-5.el7.x86_64
``` 