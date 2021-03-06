# Store the values for coritical resources as a function of the position along the horizontal meridian.

isom <- R.matlab::readMat('~/Dropbox/Calen/Work/search/gain_map/isod.mat')$isom
xij  <- R.matlab::readMat('~/Dropbox/Calen/Work/search/gain_map/xd.mat')$xijm


ecc <- xij[780, 1:353]
resource <- unlist(map(seq_along(ecc), function(x) sum(isom[, x] != 255)))

neural_resource <- data.frame(eccentricity = ecc, neural_resource = resource)


save(file = '~/Dropbox/Calen/Work/search/gain_map/neural_resource.rda', neural_resource)