# Fake File System

A virtual file system. Maps data operations onto a file system inside a single file.


## Building

This project requires SBT 0.13

## Try it out

    > sbt console
    
On the sbt console, you are provided an `fs` object that you can run some commands on.

## File system format

Kind of like the Unix File System, we save everything in fixed size blocks of 512 bytes.
 
Block types:

* header block - just one block at the beginning of the file (address 0). Contains metadata and addresses of root directory.
* directory block - contains file entries (filename and metadata). these can be directories or actual files
* file block - references to actual file data blocks
* data block - raw data for files

## Limitations

* Blocks have an integer address, so at most we can save Int.MaxValue * 512 bytes in a Fake File System.
* Files have only one block for their addresses, so max file size is limited to a little less than 64k and files in a directory are limited to at most 4032
* file names are at most 8 bytes
* not thread-safe: multiple instances of an FFS can be created for the same file. As they cache data, it can become out of sync easily.
* performance was not a consideration in the design


## Design

The user facing API is the FFS class/object, which allows opens a FFS from a physical file, and allows some basic
operations. All operations directly modify the underlying file, as long as FFS is used and not the underlying library.