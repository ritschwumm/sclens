name			:= "sclens"

organization	:= "de.djini"

version			:= "0.11.0"

scalaVersion	:= "2.10.2"

libraryDependencies	++= Seq(
	"de.djini"		%%	"scutil"	% "0.24.0"	% "compile",
	"org.specs2"	%%	"specs2"	% "1.14"	% "test"
)

libraryDependencies	+= "org.scala-lang" % "scala-reflect" % scalaVersion.value

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
