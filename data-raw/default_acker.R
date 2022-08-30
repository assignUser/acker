default_acker <- get_acker_vect(395695875)
# Soil profile from https://nibis.lbeg.de/cardomap3/?TH=510#
# "Braunerde" Profil 57 https://www.lbeg.de/extras/nlfbook/html/prof0057.htm
default_soil <- iCrop2R::german_soil("Braunerde", 41, 10, 16, 15, 100)


terra::writeVector(default_acker, "data/default_acker")
usethis::use_data(default_soil, overwrite = TRUE)
