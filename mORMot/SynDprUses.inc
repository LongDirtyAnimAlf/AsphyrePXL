{$ifndef LVCL}
  {$ifdef FPC} // we may be on Kylix or upcoming Delphi for Linux
    {$ifdef Unix} // we may also be on Darwin / OSX
      cthreads, // should be the very first unit - before FastMM4 e.g.
    {$endif Unix}
    {$ifdef FPC_FASTMM4}
      fpcFastMM4Messages in '..\fpcfmm4\fpcFastMM4Messages.pas',
      fpcFastMM4 in '..\fpcfmm4\fpcFastMM4.pas',
    {$else}
      {$ifdef FPC_SYNJEMALLOC}
        SynFPCCMemAligned, // after "apt-get install libjemalloc1"
      {$else}
        {$ifdef FPC_SYNTBB}
          SynFPCCMemAligned, // after "apt-get install libtbb2"
        {$else}
          {$ifdef FPC_SYNCMEM}
            SynFPCCMemAligned, // you may also set FPC_NO_DEFAULT_MEMORYMANAGER
          {$else}
            {$ifdef FPC_CMEM}
              cmem, // you may also set FPC_NO_DEFAULT_MEMORYMANAGER
            {$endif FPC_CMEM}
          {$endif FPC_SYNCMEM}
        {$endif FPC_SYNTBB}
      {$endif FPC_SYNJEMALLOC}
    {$endif FPC_FASTMM4}
    {$ifdef Unix} // we may also be on Darwin / OSX
      {$ifdef Darwin}
        iosxwstr, // optional WideString manager for Mac, but won't hurt
      {$else}
        cwstring, // optional WideString manager, but won't hurt
      {$endif Darwin}
    {$else}
      {$ifopt D-} // at the moment, debug mode with cmem causes trouble
        //cmem, // default FPC's heap manager is very RAM hungry (one heap per thread)
      {$endif}
    //FastMM4,  // need the latest version, e.g. from lib\kylix sub folder
    {$endif Unix}
  {$else}  // enable FastMM4 on older versions of Delphi
    {$IFDEF CONDITIONALEXPRESSIONS}
      {$if CompilerVersion <= 17} // Delphi 2006 (and up) have FastMM4 integrated
      FastMM4Messages,
      FastMM4,
      {$ifend}
      {$ifdef UNICODE}
        {$WARN DUPLICATE_CTOR_DTOR OFF} // global at .dpr level - see QC 100815
      {$endif}
    {$ELSE}
      FastMM4, // no conditional expressions -> versions older than Delphi 6
    {$ENDIF}
  {$endif FPC}
{$endif LVCL}

