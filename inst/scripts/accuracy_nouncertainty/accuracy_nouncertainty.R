# No Uncertainty D'
dpmap_rcw      <- R.matlab::readMat('./rcw_dpmap.mat')
dpmap_anqi     <- R.matlab::readMat('./anqi_dpmap.mat')
prior_uniform  <- R.matlab::readMat('./prior_uniform.mat')
prior_polar    <- R.matlab::readMat('./prior_polar.mat')

dpmap_rcw      <- dpmap_rcw$dpmap
dpmap_anqi     <- dpmap_anqi$dpmap
priorh_uniform <- prior_uniform$priorh
priorh_polar   <- prior_polar$priorh


pc_rcw_uniform  <- sum(pnorm(dpmap_rcw/2) * priorh_uniform)
pc_anqi_uniform <- sum(pnorm(dpmap_anqi/2) * priorh_uniform)

pc_rcw_polar  <- sum(pnorm(dpmap_rcw/2) * priorh_polar)
pc_anqi_polar <- sum(pnorm(dpmap_anqi/2) * priorh_polar)
