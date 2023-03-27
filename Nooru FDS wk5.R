library('multigraph')

# http://vlado.fmf.uni-lj.si/pub/networks/data/ucinet/ucidata.htm#thuroff
# https://github.com/mplex/multigraph

Thurman <- multiplex::read.dl(file = "http://vlado.fmf.uni-lj.si/pub/networks/data/ucinet/thuroff.dat")

scp <- list(cex = 3, fsize = 8, pch = c(19, 15), lwd = 1.5, vcol = 2:3, fsize = 7)
# Multigraph with customized format
multigraph(Thurman, scope = scp)
# Bipartite multigraph for actors affiliated by different means in Thurman Office
bmgraph(Thurman, ecol = 1)
# Multigraph with actors in two columns in the Thurman Office decision network
bmgraph(Thurman, layout = "bip3", scope = scp)

