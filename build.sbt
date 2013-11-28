name			:= "sclens"

organization	:= "de.djini"

version			:= "0.5.0"

scalaVersion	:= "2.10.1"

libraryDependencies	++= Seq(
	"de.djini"		%%	"scutil"	% "0.19.0"	% "compile",
	"org.specs2"	%%	"specs2"	% "1.14"	% "test"
)

libraryDependencies	<+= (scalaVersion) { "org.scala-lang" % "scala-reflect" % _ }

scalacOptions	++= Seq(
	"-deprecation",
	"-unchecked",
	// "-language:implicitConversions",
	// "-language:existentials",
	// "-language:higherKinds",
	// "-language:reflectiveCalls",
	"-language:dynamics",
	// "-language:postfixOps",
	// "-language:experimental.macros"
	"-feature"
)
