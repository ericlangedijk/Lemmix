{-------------------------------------------------------------------------------
  #0: some program info
-------------------------------------------------------------------------------}
{$define beta} // inserts 'beta' into program name

{-------------------------------------------------------------------------------
  #1: decrease exe size
-------------------------------------------------------------------------------}
{$weaklinkrtti on}
{$rtti explicit methods([]) properties([]) fields([])}

{-------------------------------------------------------------------------------
  #2: compiler directives. do not change these
-------------------------------------------------------------------------------}
{$pointermath on}                 // must be on
{$rangechecks off}                // must be off. on = impossible
{$overflowchecks off}             // on = possible but not really useful
{$scopedenums on}                 // must be on. we like this

{-------------------------------------------------------------------------------
  #3: release / debug options
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
    {.$define paranoid}        // define this to activate some very lowlevel checks including lowlevel asserts. slows down of course.
    {$inline off}
    {$optimization off}
    {$debuginfo on}
    {$stackframes on}
    {$assertions on}
    {$referenceinfo on}
  {$ifend}
{$ifend}

{-------------------------------------------------------------------------------
  #4: debug logging
-------------------------------------------------------------------------------}
{$ifdef debug}
  {.$define logging}
  {.$define log_remove}
  {.$define log_transitions}
  {.$define log_turns}
  {.$define log_commands}
  {.$define log_assignments}
  {.$define log_traptriggering}
{$endif}
