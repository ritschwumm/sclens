name			:= "sclens"

organization	:= "de.djini"

version			:= "0.28.0"

scalaVersion	:= "2.10.3"

libraryDependencies	++= Seq(
	"de.djini"			%%	"scutil"		% "0.40.0"				% "compile",
	"org.scala-lang"	%	"scala-reflect" % scalaVersion.value	% "compile",
	"org.specs2"		%%	"specs2"		% "2.3.7"				% "test"		exclude("org.scala-lang", "scala-library")
)

scalacOptions	++= Seq(
	"-deprecation",
	"-unchecked",
	// "-language:implicitConversions",
	"-language:existentials",
	// "-language:higherKinds",
	// "-language:reflectiveCalls",
	"-language:dynamics",
	"-language:postfixOps",
	// "-language:experimental.macros"
	"-feature"
)
