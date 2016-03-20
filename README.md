# Fake File System

A virtual file system. Maps data operations onto a file system inside a single file.


## Building

This project requires SBT 0.13

## Try it out

    > sbt console
    
On the sbt console, you are provided an `fs` object that you can run some commands on.

## File system format

Kind of modeled after the Unix File System. 
All the data and metadata is stored in fixed size blocks of 512 bytes.
 
Block types:

* header block - just one block at the beginning of the file (address 0). Contains metadata and addresses of root directory.
* directory index block - references directory blocks
* directory block - contains file entries (filename and metadata). These can be directories or actual files
* file block - references to actual file data blocks
* data block - raw data for files

## Limitations

* Blocks have an integer address, so at most we can save Int.MaxValue * 512 bytes in a Fake File System.
* Files have only one block for their addresses, so max file size is limited to a little less than 64k and files in a directory are limited to at most 4032
* file names are at most 8 bytes
* performance was not a consideration in the design
* the API is pretty basic


## Design

The user facing API is the FFS class/object, which allows opens a FFS from a physical file, and allows some basic
operations. All operations are directly reflected in the underlying file. Bad parameters and errors are mostly signaled
through exceptions instead of types and return values :(

### Thread safety

Because it operates on physical files with strict evaluation semantics,
we can't really achieve thread-safety by immutability. Instead, we use file locks to block other processes
from accessing the file externally (hopefully) and have only one instance per physical file. This is ensured by a
private constructor and factory methods. If the underlying file is modified not through FFS, all bets are off.
