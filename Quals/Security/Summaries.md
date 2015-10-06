# Security Qual Paper Summary

## Inside the Slammer Worm

Describes the speed and nature of the spread of the Slammer Worm that
occurred on January 25th, 2003.

Slammer Worm:
* Infected over 90% of vulnerable computers (at least 75,000) within
  10 minutes of release.
* Took only 3 minutes to achieve full scan rate.
* Vulnerability was a flaw in Microsoft SQL server that had been fixed
  back in July 2002 -- vulnerable hosts hadn't updated.
* No malicious payload -- considerable harm caused though as
  overloaded a large number of links, routers and hosts on the
  Internet (DoS).

Scanner:
* Random scanner -- randomly generates an IP and tries to infect the
  host (potentially) at that IP. Initially spread exponentially, then
  drastically fall of as they reach saturation due to retrying same
  addresses.
* Other random scanners have been orders of magnitude slower in past,
  however Slammer was only 376 bytes, so can send in a single UDP
  packet, making its scan rate per machine very high (i.e., bandwidth
  limited -- 26,000 on 100M link).
* Most other worms use TCP and are fairly simplistic, spawning a few
  threads but waiting for time-outs if a machine doesn't exist -- they
  could avoid this though, so TCP could be used as effectively as
  Slammer used UDP.
* Scanner uses a PRNG. However, Slammer authors made some mistakes so
  they're PRNG was flawed and didn't scan some addresses ever. Used of
  good quality seeds meant that over many instances, most of these
  deficiencies were irrelevant. This also made analysing Slammer
  harder -- can't figure out the address ranges it tried scanning.

Response:
* Within a few hours many sites had begun filtering UDP 1443. However
  this did nothing to limit infection (occurred within 10 minutes) and
  simply dealt with the effects of the scanner load on the Internet.
* Filtering was easy due to a clear signature (UDP -- 1443) and the
  use of a uncommon port / service (Microsoft SQL Server).

Problems Caused:
* No malicious payload. But load brought down one county 911
  emergency's data-entry terminals and portions of Bank of America's
  ATM network.
* Issues simply caused by load -- effectively DoS.
* Networks should employ traffic shaping, fair queueing or similar
  techniques to prevent domination of network by a few.

Outcomes:
* Demonstrates speed of infections and of scanning.
  * Human responses vastly insufficient -- need automated defences to
    high speed worms, especially as a more sophisticated worms may
    stop scanning after infection to avoid detection and remain
    dormant.
  * Smaller populations (i.e., 20,000) for infection are now a viable
    target using better scanning techniques as exemplified by Slammer.

Me:
* IPv6 makes search space too large to effectively scan / randomly
  probe.
* Fibre at home though makes a infected host far more valuable and
  powerful.
* Improving (potentially enforcing) automatic updating (and
  live-updating) of software extremely useful -- i.e., a huge amount
  of viruses and worms exploit known, patched vulnerabilities, not
  0-days.


## Hunting For Metamorphic

Describes the state of virus scanners and virus writers with a
historical view of changes and the challenges being faced now and in
the future.

Simple Virus:
* Straight code with no defensive measures (against virus scanners).
* Easy to detect by simple string matching.
* Put virus into code by replacing usual entry point with virus entry
  point (i.e., so virus scanner knows where to look).

Virus Scanners:
* Ideally find a infection in a program through simple wild-card
  string search. Fast, often accurate.
* Can only detect viruses they know about, reactionary defense, no
  preventative measures.
* So virus writers look for ways to disguise a virus from scanners
  (i.e., how to make simple known-string detection hard)

Encrypted Viruses:
* Virus = <constant decryptor> : <encrypted virus body>
  (Unencrypted virus body is constant).
* Idea: virus body encrypted (use non-deterministic encryption) so it
  can mutate on each infection.
* Scanner: Can usually deal with fine by just matching on the constant
  decryptor OR, can match on that and if not unique enough yet, try
  decrypting the virus body and matching on constant unencrypted form.

Oligomorphic Viruses:
* Virus = <permutable decryptor> : <encrypted virus body>
  (Unencrypted virus body is constant).
* Number of variations of decryptor is constant.
* Scanner: Decrypt virus body and match on constant unencrypted form.

Polymorphic Viruses:
* Virus = <random (polymorphic) decryptor> : <encrypted virus body>
  (Unencrypted virus body is constant).
* Number of variations of decryptor is infinite.
* Scanner: Sometimes possible to match on wild-card string still,
  other times use a code emulator to run code and match on known
  configuration state string, or try dynamic decryption and matching
  on constant body.
* Virus writers will try anti-emulation techniques (i.e., odd, rare
  instructions).
* Surprising low number of efficient external polymorphic engines.

* Stack rebuild: Virus decrypts itself on the stack (e.g.,
  polymorphic) and supports jump insertion and removal between any
  instructions of the build code.
* Scanners: Can deal with easily with code emulators -- match constant
  decrypted virus body on stack.

Metamorphic Viruses:
* Virus = <polymorphic unencrypted virus> (i.e., able to mutate its
  own code).

* Most polymorphic viruses eventually decrypt themselves to a constant
  body. Metamorphic viruses don't.

* Source Code: Virus caries source code around with it, if host has a
  compiler then it mutates the source code and recompiles.  E.g.,
  common among macro and script viruses.
* Register Exchange: Code remains constant but registers used can be
  mutated to change code.
* Subroutine Reorder: Reorder subroutines.
* Scanners: Both register exchange and subroutine reorder can
  generally be detected by wild-strings but at worst case some may
  require algorithmic detectors.

* Permutation: E.g., randomly reorder groups of instructions and
  insert / remove jump instructions as appropriate to keep control
  flow order the same.

* Mutate infected: Can mutate not just virus code but also code of
  infected file (most viruses are imperfect and so have a chance of
  breaking some files through this). Makes repair by scanner very
  hard (or impossible).

* Random entry point: Most viruses run their code at the entry point,
  some random insert into the control flow and so may or may not
  actually run but then require searching the whole file to detect.

Anti-scanner Techniques:
* Do nothing: Virus inserts a random "do nothing" code block at entry
  point to challenge emulator speed.
* Sometime run: E.g., only run virus code on certain days or under any
  variable condition. Combined with other techniques make detection
  using emulation difficult.

Metamorphic Detection:
* Shape detection: detect structure -- e.g., fixed size data or code
  sections -- may be false positive but can act as a effective filter
  to eliminate potential files from further searching.
* Disassembling: Disassemble code to assembly pneumonic -- can often
  be more effective to wild-string match on this.
* Emulators and tracing: Emulate code and match on traced instruction
  history.

Future:
* Worms: These techniques have mostly been applied to viruses but
  represent a far greater threat in the form of worms.
* Collaboration: Virus writers have typically been lone wolfs but are
  increasingly forming communities and releasing frameworks and tools
  for virus development.
* Updating: Viruses now support updating themselves over the network
  and back-doors for control from the writer (e.g., botnet now with
  update feature).

Thoughts:
* Code signing? I.e., when I install code it comes with signed
  signatures from the publisher of what the code is. Updates also are
  signed and update the signature of the code. Code not conforming to
  a signed signature isn't run. Need to secure things like PDF (fonts)
  from allowing code execution in a file format. JIT also a problem
  still and buffer overflows in terms of just executing a live virus
  which could potentially use privileges to install itself, disable
  code signing... etc.
* Only reactionary and limited by commercial interests. How to protect
  one persons computer from a targetted attack?


## Buffer Overflow: Attacks and Defenses for the Vulnerability of the
   Decade

Describes the various types of buffer overflow attacks and the various
defensive techniques available at the time (circa 1999).

Buffer Overflow Attack:
* Attacker gives input to a networked program that causes it to write
  that causes it to write outside the bounds of a buffer and corrupt a
  code pointer. (e.g., user input written to a buffer with no length
  checking, overflows and corrupts an adjacent code pointer).
* Corrupting code pointer allows attacker to control where the control
  goes to, which may be injected code (e.g., into the buffer) or may
  be existing code (usually where the arguments have been hijacked by
  the attacker [e.g., exec()]).

Attacks:
* Stack smashing -- overflow a stack allocated buffer, corrupting
  return address of the stack frame / activation record.
* Function pointer -- function pointer on heap, overflow an adjacent
  buffer to corrupt.
* Longjmp buffers -- buffer used by C for setjmp(buffer) and
  longjmp(buffer). Can corrupt to change longjmp return addresses.
  These buffers can be allocated anywhere so the attack is similar to
  function pointers.

Defenses:
* Correct code -- write correct code. Basically impossible, although
  increasingly compilers can help by offering warnings and static
  analysis for potential problems. (e.g., warn if strcpy is used vs.
  strncpy).
* Non-executable buffers -- Use non-executable bit on stack.
  Traditionally not used due to some rare cases where dynamically
  generated code is placed on the stack. However, largely this
  technique is used these days to stop attacking code being placed in
  stack allocated buffers.
* Bounds checking -- Check all array access according to bounds. Done
  by all memory safe languages. Hard to do with C while maintaining
  performance, compatibility and complete coverage (i.e., can't really
  achieve all three).
* Type safe languages -- i.e., all do array bounds checking. Languages
  rely less on explicit pointers so encourage a coding style that
  suffers less in performance from bounds checking.

* Code pointer integrity checking -- Don't prevent corruption of
  pointers *but* detectable when it occurs.
  * StackGuard: Place a unique word (canary, randomly generated on
    process startup) at the start of each stack frame. Check it is
    still valid before returning. One word overhead per stack frame,
    function calls more expensive but macro benchmarks don't show
    performance loss for network services. (probably IO and
    concurrency bound than anything else). Only protects attacker
    placing own code pointers on stack though. Very high compatability
    with existing code (recompilation needed).
  * PointGuard: Generalization of above to try to protect all code
    pointers (so heap and static data as well). Places a canary next
    to all code pointers -- hard to do as must special case all
    potential pointers and data structures for compatability reasons.
    Need to check canary on all pointer dereference (performance --
    conflict with some optimizations like moving a read up in the
    program and placing in a register...). Should provide complete
    protection against buffer overflows.

* Combination -- these defenses can often be used together (e.g.,
  non-executable bits with any of them).

Linus:
* Stack non-exec -- complains its trivial to attack a buffer overflow
  by calling existing code so non-exec does nothing really. However,
  doesn't ASR complicate calling existing code enough to negate this
  complaint?

My Thoughts:
* Split stack -- have two stacks that operate in unison. Any
  potentially overflow-able stack allocated structure is placed on the
  'buffer' stack. All other variables and return addresses are placed
  on the 'fixed write' stack. Wouldn't need to check return address is
  valid on return (could never be corrupted) but stack manipulation is
  a little more expensive now. Also, doesn't stop one buffer
  corrupting a buffer in a previous stack frame.
* Layout randomization -- Attacker corrupts heap allocated code
  pointers or longjmp buffers by finding an adjacent buffer to
  overflow. So can we simply have the compiler randomize on each
  compilation how it layouts the heap?
* Pointer tagging -- pointer tag for the canary instead of separate
  word. Probably more expensive actually and not enough entropy in the
  tag bits (since too expensive to do tag bit choice dynamically, so
  choice done at compilation). Although, I believe current 64bit
  hardware only actually interprets 64bit points using 42 bits.
  Potentially the architecture could agree to never use more than say
  50 bits and then at least the canary has 16k choices, so gets
  reasonable.. Potentially hardware that supports tagging could be
  used (future).

Now:
* StackGuard lost out to ProPolice (IBM) -- similar in nature but also
  rearranges how variables and buffers are laid out on the stack to
  offer some protection of saved registers and others from corruption.
  Implemented in GCC mainline since 4.1. Many distributions enable it
  by default. Visual studio since 2003 has a stack protector that is
  enabled by default.
* LLVM doesn't have a stack guard but instead has two different static
  analysis (and dynamic analysis) tools that can catch buffer
  overflows and other vulnerabilities (SAFECode and AddressSanitizer).
* ASLR -- enabled by default on Windows, OSX, OpenBSD, Linux [weak
  form -- not much entropy], Android and iOS.
* NX Stack -- Supported on OSX, Windows, Netbsd, OpenBSD, Android
* NX Heap -- Windows, OSX, NetBSD, OpenBSD, Android


## Reflections on Trusting Trust

* 1984, Turing award lecture

Demonstrates how a backdoor can be inserted into software such that it
is carried around in the binary only and never present in the source
code. Argues that perhaps the people writing your software need to be
trusted, not the software itself.

Technique:
* Bootstrapping compiler -- at one point have explicit code to handle
  a construct in the language, but after that can now use that
  construct in the defition itself.
  i.e., Phase 1: '%n' => 79
        Phase 2: ''%n' => '\n'
  This works as the initial definition is embedded in the binary
  still, kind of a recursive definition back to the original source
  code.
* Key point is we've made a self-reproducing knowledge / program where
  the knowledge isn't visible in the source code.
* Can insert a backdoor this way -- present in binary, detects a
  certain function and inserts itself (and inserts into compilers it
  compiles), but not present in source code so can't verify through
  source code inspection unless the compiler we compile with is
  trusted.
* Could have used any tool in the compilation tool chain -- assembler,
  linker, OR even _hardware microcode_.

Observations:
* 1984! And it really is describing a self-replicating virus with
  whatever payload you want (i.e., backdoor). Outlines how this
  general idea can be applied throughout the stack, even hardware!
* No solutions proposed.

Questions:
* Based on references it seems this is the first outline of viruses
  and trojan horses? If so, the coverage of the key ideas
  (replication, obfuscation, microcode...) is amazing.

Next steps:
* Hard. Outlines an entire, large branch of security research.
  However, does point towards verification as the ultimate and only
  true solution. I.e., if in microcode or compiler how to trust? Need
  whole stack verification -- or at least verification of the TCB.
* Easier solutions?
  * MAC?
  * Logging program behaviour for suspicious behaviour (i.e., reverse
    intrusion detection)
  * Use of multiple tool chains and simultaneous execution (combined
    with logging can compare for differences).
  * Fuzz test tool chain against each other?
  * Fully homomorphic encryption? We'd need to trust our server for
    encrypting the data (and the software, compiler, OS that allows us
    to do that) but not any of the stack on which the computation
    runs. So potentially can have a very tiny toolchain just for
    encryption.


## Efficient Software-Based Fault Isolation (SFI)

*  1993, ACM SIGOPS

Rather than use hardware (address spaces) to isolate software we can
do it purley in software. I.e., software protection domains. Trade-off
of more expensive execution time for all code but fast communication
between domains (i.e., since no context switch -- single address
space).

Goal:
* Make protection (isolation) domains cheap enough for programs to be
  privilege separated. I.e., choose domains for security, not
  performance.
* I.e., sandboxes!
* Trades execution time for RPC speed Vs. Hardware.

Context:
* Classic paper. Establishes the whole domain of SFI, protection
  through software instead of hardware. Single address-space
  advantages.
* Follow on: NaCl, Singularity, Dune! (makes the hardware isolation
  cheap), Chrome. Java Stack Inspection, Safe Haskell.

Approach:
* Load a module into own domain (extent of the address space + UID).
* Segments are aligned such that all code in a segment shares same
  unique upper N bits.
* Rewrite code (binary translation) to ensure it can't escape its
  fault domain (not PL specific):
  * Uses 7 dedicated registers -- problem on X86 with its limited
    register set BUT x86 segmentation hardware can be used for at
    least 2 of them (and can reduce further).
  * Statically sandbox known addresses (load time).
  * Need to sandbox jumps through registers (unknown addresses) -- use
    decicated registers to sanitize addresses to be within the domain
    space (i.e., masks, shifts, traps if outside).
  * Require untrusted modules invoke system calls through RPC calls to
    a trusted security monitor. (transform syscalls into RPCs).
  * Use trusted call-return stubs (per pair of fault-domains) and a
    read-only jump table of addresses for escaping a fault domain (can
    only escape to these addresses).
  * Call-return stubs are essentially performing a light-weight
    context switch.
* No hardware support needed! Could be used on machines without MMU.
* Doesn't allow code modification, so wouldn't work with JITs.
* Can share memory by using MMU to map shared segments into each fault
  domain -- map at same offset in each segment so lower order bits are
  the same [so requires all domains be same size].

Implementation:
* Compiler modification + verifier (load time) or binary translation.
  They use the former approach (gcc) [as does NaCl].
* Although, how to remove use of dedicated registers in binary
  translation? Seems difficult, or at least generating significantly
  worse code (lots of pops, pushes).

Use cases:
* Assume the use will be for loading untrusted modules in largely
  trusted code bases. (e.g., plugins for Postgres). As only the
  untrusted code pays the execution penalty but RPC's may be very
  common, good trade-off Vs. hardware.

Evaluation:
* Nice! Analytical model and  no hand-waving. Quantify explicitly
  where performance was lost and won.
* Execution overhead:
  * Fault-Isolation: 4.3% (no protection as domains can read whole
    address space).
  * Protection: 21% (isolation).
* RPCs:
  * 2.83us VS. 204us (2 orders of magnitude).
* Analytics model predicts accurately! -- if spend 30% of application
  time crossing fault domains need a 10% improvement to RPCs to win,
  if 10% crossing, then 39% improvement to RPCs.

Downsides:
* Don't sandbox loads by default -- untrusted code can read whole
  address space.
* Doesn't protect against *buffer overflows* -- could imagine doing live
  binary translation (i.e., Vmware) to handle this.
* How to deliver signals? signals are on a process level, so how to
  map to fault-domains -- maybe multiple want to receive same signal?
* Blocking system calls? I.e., `accept()`.

Follow On:
* OK, protection / fault domains cheap now what?
  * How to structure programs? Privilege separation, security
    monitor, protecting data? start getting into capabilities, IFC.
  * Buffer overflows? Work is addressed towards fault isolation, so
    probably other examples of security flaws not handled.


## Bro: A System for Detecting Network Intruders in Real-Time

* 1998, Computer Networks

Describes a standalone network intrusion detection system that
operates in real-time. Doesn't require modification of applications to
protect them.

Goals:
* Passively watch DMZ link, 100mb/s line.
* No dropped packets -- easy to exploit otherwise.
* Real-time notification -- still keep logs for offline.
* Policy and mechanism separated -- extensible.
* Monitor will be attacked -- protect self. (Does assume that at least
  one end-point will be trusted).

Approach:
* Pipeline: libpcap -> event engine (FIFO) -> policy script engine.
* Capture all packets.
* Scripting language (Bro): DSL, strongly typed.
* They do a process checkpoint operation periodically. Seems to be a
  foolish way to do GC. (Although could claim no pause times).
  Checkpoints are also for saving data for off-line analysis but this
  isn't motivated well either.

Policies:
* Pretty boring -- also trying to reconstruct application level
  protocol. Better done as a application/language level framework?
  I.e., a security monitor. Especially given that a third-party
  writing a Bro script for any complex protocol seems difficult.
* On a university network they get 40-realtime notifications per day!
  For any big site the number of notifications and false-positives
  seems overwhelming.

Paper:
* So boring! Terrible paper name.
* Spend half of it talking about the design of the Bro language, which
  is completely pedestrian DSL. Maybe in 1998 using a DSL was a little
  more novel though...

Thoughts:
* Assumption of trust in one end-point problematic. Ideally we'd like
  it to detect when a botnet client has been installed on a host but
  that is outside the assumptions. Or, a lot of attacks are actually
  carried out by insiders.
* Assume that an attacker doesn't have access to policy scripts! This
  is their claim for defence against overload attacks as the attacker
  doesn't know what events will go up to the slower scripting layer.
  This assumption is very unrealistic for any serious deployment.
* Checkpoints introduces a potential flaw -- suspicious but not yet
  raised network activity is lost over a snapshot as that internal
  state is blown away.
* If you can crash Bro it relaunches (duh) but window of where
  unmonitored that you can attack. (Relaunches are logged obviously).
* Bro can't detect on protocols that use encryption.
* General goal seems incorrect. Rather than try to protect known
  protocols run on servers on your machine you should be trying to
  detect unexpected flows. There policies do things like filter
  usernames and potential buffer overflows from telnet. This kind of
  goal seems better done with a server side security monitor. E.g.,
  Histar, SELinux, AppArmour. E.g., how would this protect against SQL
  injection attacks easily? However, assuming both hosts are untrusted
  and watching for flows not expected (i.e., wrong direction for
  initiation, wrong port / protocol). I.e., perhaps don't do
  application level protocol inspection or just look for statistical
  anomalies over time.
* The protection they offer seems better done through a MAC system,
  better programming practices and tools (protection domains, static
  analysis, stack guard, dynamic tain analysis... e.t.c.).


## Using Programmer-Written Compiler Extensions to Catch Security
   Holes

* 2002, IEEE Symposium on Security and Privacy
* Previous work look at catching general bugs with static analysis and
  extensions, not specifically security.

Demonstrate the use of system specific static analysis to catch real
security vulnerabilities. Programmer can extend the static analysis
easily in a DSL (basically analysis does dataflow analysis) to flag
security flaws.

Approach:
* DSL for specifying dataflow analysis -- can pattern match on source
  code for source, sinks, sanitizers -- dataflow analysis figures out
  if an untrusted source ever flows to a sink without sanitization.
* Match on source level! Bad in sense that types (even as annotations)
  are nice as document the code, but this also makes it easy to adapt
  to existing large code bases and arguably less error prone as can't
  forget an annotation. (call it 'belief analysis').
* Nice 'belief analysis' approach -- i.e., if we sanitize some data
  but it didn't come from what we thought was an untrusted source, then
  flag this as perhaps other paths from that (maybe) untrusted sink
  that aren't sanitized. OR if the sanitized data doesn't go to a sink
  we know about, flag it as maybe other paths to this unknown sink.
* Rank errors so programmer can concentrate on most likely flaws
  first.
* Can carry across false-positive information from one run to the
  next. Stored in logs.

Thoughts:
* Real! Catch over 100 security errors in Linux and OpenBSD. Many of
  them very serious (i.e., root control).
* False positive rate of around 20% but total errors generated low so
  manageable.
* Few of the rules were about overflow behaviour -- primitive
  successor to KINT and now STACK?
* They have a fairly convincing argument of a static analysis
  (out-of-band) approach Vs. Type systems: non-invasive (i.e., no
  annotations needed), easy to adopt with existing code bases, many
  dynamic enforcements (bounds checking) that generate exceptions are
  notoriously mishandled, extension based approach easier to combine
  with ad-hoc knowledge of the systems (e.g., mark of functions of
  form `sys_*`).
* Downside to above is the lack of documentation that types provide,
  the lack of constant feedback, easier to not update the static
  analysis as code changes, easier to violate it with new sinks or
  sources (although belief analysis helps a lot here). Also, major one
  would be the ability to reason and verify the code as correct. A
  strong type system greatly simplifies this process.
* Static analysis may create a false sense of security when using
  languages with notorious security flaws like C.
* Analysis is one source code -- issue with compilers and
  optimizations -- various checks may be removed or re-ordered (e.g.,
  concurrency). Type systems are part of the compiler so don't suffer
  this problem.


## Traps and Pitfalls: Practical Problems in System Call Interposition
   Based Security Tools

* 2003, The Network and Distributed System Security Symposium (NDSS)

Discusses the issues faced in building a system call interposition
based security tool called Janus. Basically, the POSIX API is hard to
sandbox as it's hard to parse. Relative filenames, symlinks, TOUTOC
bugs...

Janus:
* Ptrace originally, now a kernel module that calls out to a user
  space policy engine that implements a users policy.

Pitfalls:
* Incorrectly replicating OS state (i.e., we think a socket is UDP,
  what does the OS think? have you handled a `dup2` statement? many
  ways to state transition in POSIX, hard to remember to cover all of
  them) -- Solution: query kernel instead (both for state and answers
  [e.g., resolving relative file names]).
* Indirect paths. Many ways to name the same underlying resource in
  POSIX. Relative paths + symlinks + mount points = pain. Can also
  pass fd around on sockets, any application can create core dumps,
  e.t.c. Can coerce another process into doing your bidding (confused
  deputy problem) -- Solution: none, just be careful.
* Race conditions. Time-of-check/time-of-use bugs. Symlinks, relative
  paths, mount points. Argument races (if reside in untrusted address
  space). -- Solution: copy arguments to protected space. Restrict
  style of paths applications can use to make them easy to verify
  (will restrict some safe ones as well sadly). (via shared lib
  replacement).

Limitations:
* Doesn't support multi-threaded applications.

Takeaways:
* System call interposition is hard -- don't trust even your security
  tools.
* Linux / POSIX API could be much better designed with security in
  mind (or just better specified).
* Don't duplicate the work of the kernel -- i.e., let it resolve
  relative paths.
* Approach flawed in general? Lots of issues with filesystem and
  having sandboxed apps share the same address spaces as the system we
  are protecting. Better to separate apps rather than filter. I.e.,
  the whole LXC approach of extending Linux API's, allowing to create
  new filesystem namespaces, network stacks... e.t.c., virtualize.
  DUNE also enables a more powerful toolset -- by itself though
  doesn't solve the pitfalls.


## Dynamic Taint Analysis for Automatic Detection, Analysis, and
   Signature Generation of Exploits on Commodity Software

* 2005, The Network and Distributed System Security Symposium (NDSS)

Propose a dynamic taint analysis (through binary translation) for
automatic detection of overwrite attacks (i.e., buffer overflows).
Works with unmodified binaries and can also generate signatures for
detection of attacks (i.e., for intrusion detection systems or virus
scanners) -- specifically geared to stop the fast spread of worms
without (since not feasible) human intervention.

Signatures are 'content-based' signatures that are used to pattern
match on packet payloads.

Advantages:
* Works with existing OS and binaries.
* Detects most overwrite attacks: format string, buffer overflows...
  i.e., technique is independent of the overwriting method.
* No known false positives!
* Automatic signature generation.

Disadvantages:
* Slows server execution between 1.5 and 40 times.

Key observation:
* 'For an attacker to change the execution of a program
  illegitimately, she must cause a value that is normally derived from
  a trusted source to instead be derived from her own input'.
  * Although, what a sanitized input? e.g., jump tables.

Implementation:
* Valgrind for dynamic binary translation.
* By default network sources are untrusted (taint them!). (source)
* Logging method:
  * Each byte of memory (including registers) => 4 byte shadow memory!
    pointer (so 8 byte on 64 bit) to taint structure (page table like).
  * On tainting, new taint structure built that has snapshot of current
    stack! (and system call number + data written).
* Faster method:
  * Shadow memory is a single bit indicating the byte is tainted.
* Each instruction that propagates bytes (LOAD, STORE, MOVE, PUSH,
  POP, etc) and arithmetic instructions TainCheck adds instrumentation
  before each instruction to propagate the taint. (track / propagate)
* Check that jumps addresses, format string, system call arguments,
  and application specific checks don't use tainted bytes. (sink)
* Don't check if tainted bytes used to alter control flow (i.e.,
  branching) -- done often and unclear how to differentiate between an
  attack or safe usage.
* After violation detected, analyser provides information on how
  exploit occurred. Can even allow exploit to continue in a controlled
  environment to gain more information.

False negatives:
* Use control flow to influence a jump target. I.e., need indirect
  influence to avoid the taint.
* Untrusted sources not flagged.

False positives:
* Doesn't handle sanitization of tainted data right. Taints are
  forever! (concurrency could make this hard! Or really hurt
  performance).

Performance:
* Not clear in paper if using fast or logging method :(.
* Worse case of 36x overhead, but Apache only 1.05x. However 25ms
  impact on latency!

Usage:
* Protection of individual hosts.
* Protection / honeypot of a few hosts for worm detection and
  signature generation.

Thoughts / Next steps:
* Performance needs to be further explored and improved.
* Lack on sanitization seems a big issue for a lot of systems, need a
  way to track that efficiently and encode the programmer specific
  knowledge -- at that point potentially the fact it works with a
  binary may be mute. Either too hard to identify sanitization in
  binary or need too much knowledge so have source code anyway.
  * So take above approach but put into compiler, allow expressing
    santization, improve performance.
* Signature generation ideas are very cool, would simply like to see
  them tried and explored.


## Understanding Java Stack Inspection

* 1998, IEEE Symposium on Security and Privacy

Formalizes the Java stack inspection security model -- separate it
from just its implementation.

Java Stack Inspection:
* Fundamentally depends on Java's type safety (i.e., bounds checking,
  no arbitrary pointers, not possible to corrupt memory [rewrite
  attacks]).
* Principals -- signed units of code (applets loaded over the web)--
  so represented by a certificate--and the 'system' privilege. (Can be
  more complex than this -- can authorize principals with certain
  privileges).
* Each stack frame associated with a principal. (for where the code is
  derived, NOT the thread).
* Each stack frame contains privilege flags which may be set by a
  principle. Can also revert privileges.
* All dangerous operations invoke the stack inspection mechanism to
  decide if access should be allowed (based upon a privilege being
  enabled) [mostly about avoiding confused deputy]. i.e., code with a
  privilege (say, 'system') will have a filtered `file_open()` call
  that checks the arguments, then enables it's privilege and then
  calls the real `open()` call. Need to inspect stack to ensure that
  untrusted code hasn't fooled trusted code into doing its bidding.
* Walk stack on inspection from newest to oldest frame, if encounter
  an untrusted frame we reject, if we encounter a privilege disable we
  reject, if we only encounter trusted frames and then a trusted frame
  with privilege enable we accept.
* If we reach the end of the stack: Netscape = reject, Sun + Microsoft
  = accept.

Model:
* Model with a logic called ABLP => principals, statements (P says s
  [logical implication]), conjunction of principals (no disjunction),
  quotation (A says (B says s), may be incorrect), authority (A speaks
  for B, can be used to represent group membership, i.e., P speaks for
  Group).
* No inference rules sadly :(.
* Formal proof of termination, soundness (if decision procedure
  returns true, then there is a proof in ABLP), completeness (if
  decision procedure returns false, then there isn't a proof in ABLP)
  and equivalence with stack inspection.

Implementation improvements suggested by model:
* Can represent stack inspection as finite pushdown automaton.
* Can change to a kind of CPS style by passing the security state as
  an implicit parameter to all procedures.
  * Can implement stack inspection as a source-to-source
    transformation, so it can run on a JVM that knows nothing about
    stack inspection. Plus can apply standard optimizations now to the
    security mechanism.
  * Since state explicit (reified) can implements RPCs that pass
    across the state. (Have to trust the VMs at each end).

Thoughts:
* Paper itself is fine -- just formalizes an existing design nicely so
  not much to comment on.
* Java Stack Inspection though is difficult. It is a great achievement
  as it relies on type safety (nice marketing) and was somewhat a
  success in being a widely used system that could safely sandbox
  untrusted code (applets).
* However, notorious problems with JVM and applets. Lots of bugs in
  implementation. And the check and enabling of privileges are
  scattered all over the code base. Hard to reason about, program with
  and verify. E.g., once missing call to invoke the stack inspector is
  a security risk.
* Safe Haskell similarly relies on type safety but the separation of
  IO and pure actions with a monadic type system gives a lot of power
  for sandbox and control. Instead can use a `RIO` like concept to
  safely, with compilation, sandbox the code.
* Many other approaches with isolation that are less error prone.
* Static analysis (taint tracking) to ensure statically calls are made
  correctly to check for privileges and walk the stack?


## A Look Back a "Security Problems in the TCP/IP Protocol Suite"

* 2004, Annual Computer Security Applications Conference

Retrospective paper by the author, looking at a paper he published 15
years earlier on security issues with TCP.

Problems:
* TCP Sequence number attacks (predict to inject packets, hijack
  connections, reset them [if sequence number in whole active
  window]). Solution: cryptographic RNG for tcp sequence start. Lots
  of people just do random increments though due to fear of load of
  crypto.
* RIP attacks -- often unsecured, accepts new route information from
  any host so trivial to steal an IP. Then authentication using
  address-based is hosed.
* Authentication servers (contact a particular server to authenticate
  a incoming user is who they say they are). More secure as server
  opens a second TCP connection so less susceptible to attacks on
  address-based auth, but RIP attacks can still fool server into
  contacting wrong auth server. (TCP alone is unsuitable for
  authentication).
* Lots of reliance on address-based authentication! (i.e., packets
  from `168.12.9.*` are authorized) Which caused a lot of problems due
  to spoofing and routing attacks. Need to use actual authentication
  schemes (based on cryptography).
* Numerous routing problems -- 'A Survey of BGP Security' is a better
  look at this.
* Finger, POP, DNS, FTP, SNMP, TFTP..

Thoughts:
* This paper was a disappointment. POP, FTP, Telnet... are obviously
  unsecured protocols that aren't interesting and have easy ways to
  secure (SSL), IPSec, DNSSec.
* The more interesting errors are with actual protocol level flaws
  such as TCP and Routing where encryption doesn't solve it right off.
* I.e., more on TCP, DNS, Routing...
* Interesting how fearful everyone has been over time of the cost of
  encryption!
* Old, so no discussion of hmacs, web security, DNS cache poisoning...
* As conclusion points out, while protocol level vulnerabilities are
  very interesting a big concern, they are reasonably secure these
  days that the low hanging fruit is easily buggy code. (i.e., null
  terminator in a DNS certificate to pretend to facebook.com when
  certificate is for facebook.com\0.evil.com).


## A Survey of BGP Security

* 2005, 

<stub>

Thoughts:
* Interesting how concerned they were by crypto overheads!
* Shows how long some vulnerabilities have been known for an simply
  not fixed until a very public exploit occurs (or not even then!)
  [i.e., issues with BGP of misconfiguration by one ISP taking down
  whole internet in 1997, yet in 2005 BGP still not much more secured
  (just some filters)].

