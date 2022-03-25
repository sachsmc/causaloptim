This is a resubmission that addresses a warning when using the gcc-12 compiler

I could not reproduce the warning

Tableau.cpp:372:24: warning: ‘char* fgets(char*, int, FILE*)’ writing 1024 bytes into a region of size 257 overflows the destination [-Wstringop-overflow=]
  372 |         objGet = fgets (szObjective, LINE_LEN, p_pFile);
      |                  ~~~~~~^~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      
But I was able to reproduce this one: 

In function ‘char* fgets(char*, int, FILE*)’,
    inlined from ‘CTableau::CTableau(FILE*)’ at Tableau.cpp:372:17:
/usr/include/x86_64-linux-gnu/bits/stdio2.h:263:33: warning: call to ‘__fgets_chk_warn’ declared with attribute warning: fgets called with bigger size than length of destination buffer [-Wattribute-warning]
  263 |         return __fgets_chk_warn (__s, __bos (__s), __n, __stream);
      |                ~~~~~~~~~~~~~~~~~^~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

I believe they are both caused by the same problem, which I have fixed in this version. I can confirm that the second warning is gone in this version 0.9.2, and I hope that it also fixed the first warning. 

This was checked using R 4.1.2 and gcc 12.0.1 20220318
