name := "untitled"

version := "0.1"

scalaVersion := "2.12.8"
resolvers ++= Seq(
  Resolver.sonatypeRepo("releases"),
  Resolver.sonatypeRepo("snapshots")
)
libraryDependencies += "org.graalvm.compiler" % "compiler" % "19.0.2"
libraryDependencies += "org.graalvm.truffle" % "truffle-nfi" % "19.0.2"
libraryDependencies += "org.graalvm.truffle" % "truffle-sl" % "19.0.2"
libraryDependencies +=
   "org.typelevel" %% "cats-core" % "2.0.0-M4"
libraryDependencies += 
  "com.chuusai" %% "shapeless" % "2.3.3"

scalacOptions ++= Seq(
  "-Xfatal-warnings",
  "-Ypartial-unification"
)
//enablePlugins(GraalVMNativeImagePlugin)
//enablePlugins(UniversalPlugin)
//fork := true
//javaOptions ++= Seq(
//  "-XX:+UnlockExperimentalVMOptions",
//  "-XX:+EnableJVMCI",
//  "-XX:+UseJVMCICompiler",
//  "-Dgraal.ShowConfiguration=info"
//) ++ update
//  .value
//  .allFiles
//  .filter(_.getParentFile.getName == "19.0.2") // change this once graal 19 compiler is on maven central
//  .map(_.getParentFile.getCanonicalPath)
//  .reduceOption(_ + ":" + _)
//  .map(f => Seq(s"--upgrade-module-path=$f"))
//  .getOrElse(Seq.empty)
