{-------------------------------------------------------------------------------
  #0: some program info
-------------------------------------------------------------------------------}
{$define beta} // affects programname

{-------------------------------------------------------------------------------
  #1: compiler directives. do not change these
-------------------------------------------------------------------------------}
{$pointermath on}                 // must be on
{$rangechecks off}                // must be off. on = impossible
{$overflowchecks off}             // on = possible but not really useful
{$scopedenums on}                 // must be on. we like this

{-------------------------------------------------------------------------------
  #2: release / debug options
-------------------------------------------------------------------------------}
{$if not defined(debug)}
  {$inline auto}
  {$optimization on}
  {$debuginfo off}
  {$stackframes off}
  {$assertions off}
  {$referenceinfo off}
{$elseif defined(debug)}
  {.$define fastdebug}          // useful for debugging/checking optimizations with CPU registers, compiled code etc. it is almost like release
  {$if defined(fastdebug)}
    {$inline auto}
    {$optimization on}
    {$debuginfo on}
    {$stackframes off}
    {$assertions off}
    {$referenceinfo off}
  {$else}
    {.$define paranoid}        // define this to activate some very lowlevel checks (slows down of course)
    {$inline off}
    {$optimization off}
    {$debuginfo on}
    {$stackframes on}
    {$assertions on}
    {$referenceinfo on}
  {$ifend}
{$ifend}


