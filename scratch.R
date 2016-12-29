satchel <- Satchel$new("f1", "../testsatchel/satchel")
satchel$save(Theoph)
satchel$save(Theoph, data_name = "other")
satchel$save(Theoph, data_name = "another")
satchel$report(details = T)

satchel2 <- Satchel$new("f2", "../testsatchel/satchel")
satchel2$save(Theoph)
satchel2$save(Theoph, data_name = "Theoph2")
satchel2$report(details = T)
satchel2$available()

satchel2$use("other")
satchel2$use("Theoph", "f1")
