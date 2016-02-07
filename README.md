# Fake File System

A virtual file system. Maps data operations onto a file system inside a single file.


## Building

This project requires SBT 0.13

### Quickstart

    sbt update compile test run
    
    
## File system format

We save everything in blocks of 512 bytes. There are layout blocks and data blocks, and a header block.
All the blocks have a 2-byte "block address", contiguously numbered from the start of the file. 
This means we can have up to 65k blocks, thus our fake file system allows up to 32MB. That's not a whole lot, 
but should show the concept.  

The header block is always at the beginning and has the address 0. 

### Header block format

    # [file format marker]
    # [stuff]
    # [layout block count (1 byte)]
    # [layout block addresses (2 bytes)]...
    
### Layout block format

    # [layout block marker]
    # [file name length][file name][file block size][file block addresses]...
    
### Data block format

    # [data block marker]
    # [byte count]
    # [bytes]
    # [0-padding]

## File operations

### create
### write
### read
### move
### delete

