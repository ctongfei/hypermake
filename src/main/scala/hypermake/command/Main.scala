package hypermake.command

import hypermake.command.CmdLineAST._

object Main extends App {

  val version = "0.1.0"

  lazy val helpMessage = {
    import fansi.Color._
    import fansi.Attr._
    import fansi._

    def O(s: String) = Green(s).render
    def RO(s: String) = Blue(s).render
    def C(s: String) = Yellow(s).render

    s"""
      | HyperMake $version
      | Usage: forge [${O("options")}] [Forge script file] <${C("command")}> [${RO("running options")}] [targets]
      |
      | Options:
      |  -I <file>, --include=<file>   : Includes the specific Forge script to parse.
      |  -H, --help                    : Prints this message and exit.
      |  -O <path>, --output=<path>    : Specify the output directory. By default this is "out".
      |  -S, --shell                   : Specify outer shell to use. By default this is "bash".
      |  -V, --version                 : Shows Forge version and exit.
      |
      | Running options:
      |  -j <n>, --jobs=<n>            : Allow n jobs running in parallel at once.
      |  -k, --keep-going              : Keep going even when some jobs failed.
      |  -n, --dry-run                 : Does not actually run any recipe; just prints them.
      |  -s, --silent                  : Does not print recipes.
      |  -v, --verbose                 : Verbose mode.
      |  -y, --yes                     : Automatic "yes" to prompts.
      |
      | Commands:
      |  print <var>                   : Prints the specific Forge script variable.
      |  run <tasks or plans>          : Runs the given tasks or plans (space delimited).
      |  invalidate <tasks or plans>   : Invalidates the given tasks or plans.
      |  remove <tasks>                : Removes the output of the given tasks or plans.
      |  mark-as-done <tasks or plans> : Mark the given tasks as normally exited.
      |  export-shell <tasks or plans> : Generates an equivalent shell script that runs the given tasks.
      |
      |""".stripMargin
  }

  val cmd = CmdLineParser.cmdLineParse(args.mkString(" "))

  cmd match {
    case Cmd.Help    => println(helpMessage)
    case Cmd.Version => println(s"HyperMake $version")
    case Cmd.Run(options, script, runOptions, task) =>
  }

}
