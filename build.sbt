import AssemblyKeys._


name := "sprolog"

version := "0.1"

scalaVersion := "2.11.6"

isSnapshot := true

scalacOptions ++= Seq( "-deprecation", "-feature", "-language:postfixOps", "-language:implicitConversions", "-language:existentials" )

incOptions := incOptions.value.withNameHashing( true )

organization := "ca.hyperreal"

//resolvers += Resolver.sonatypeRepo( "snapshots" )

resolvers += "Typesafe Repository" at "http://repo.typesafe.com/typesafe/releases/"

//resolvers += "Hyperreal Repository" at "http://hyperreal.ca/maven2"

libraryDependencies += "org.scalatest" %% "scalatest" % "2.2.1" % "test"

libraryDependencies += "org.scalacheck" %% "scalacheck" % "1.11.5" % "test"

libraryDependencies ++= Seq(
	"ca.hyperreal" %% "lia" % "0.15",
	"ca.hyperreal" %% "rtcep" % "0.1"
	)
	
//mainClass in (Compile, packageBin) := Some( "ca.hyperreal.myproject.Main" )

mainClass in (Compile, run) := Some( "ca.hyperreal.sprolog.TestMain" )

//offline := true

assemblySettings

mainClass in assembly := Some( "ca.hyperreal.myproject.Main" )

jarName in assembly := name.value + "-" + version.value + ".jar"


seq(bintraySettings:_*)


publishMavenStyle := true

//publishTo := Some( Resolver.sftp( "private", "hyperreal.ca", "/var/www/hyperreal.ca/maven2" ) )

//{
//  val nexus = "https://oss.sonatype.org/"
//  if (isSnapshot.value)
//    Some("snapshots" at nexus + "content/repositories/snapshots")
//  else
//    Some("releases"  at nexus + "service/local/staging/deploy/maven2")
//}

publishArtifact in Test := false

pomIncludeRepository := { _ => false }

licenses := Seq("MIT" -> url("http://opensource.org/licenses/MIT"))

homepage := Some(url("https://github.com/edadma/sprolog"))

pomExtra := (
  <scm>
    <url>git@github.com:edadma/sprolog.git</url>
    <connection>scm:git:git@github.com:edadma/sprolog.git</connection>
  </scm>
  <developers>
    <developer>
      <id>edadma</id>
      <name>Edward A. Maxedon, Sr.</name>
      <url>http://hyperreal.ca</url>
    </developer>
  </developers>)
