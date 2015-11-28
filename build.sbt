lazy val root = (project in file(".")).
  settings(
    libraryDependencies += "com.codecommit" %% "gll-combinators" % "2.2"
  )
