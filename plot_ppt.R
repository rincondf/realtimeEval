par(mar = c(3, 6, 3, 0) + 0.1)
plot(test[[1]]$dds, test[[1]]$control, type = "l", xlim = c(150, 800), ylim = c(0, 0.0045), yaxt = "n", xlab = "", 
     cex.lab = 2, cex.axis = 2, ylab = "")
axis(2, at = seq(0, 0.0045, 0.001), labels = F)

polygon(test[[1]]$dds, test[[1]]$ref, col = mycol, border = NA)
polygon(test[[1]]$dds, test[[1]]$control, col= "brown")

title(ylab = "Relative number", cex.lab = 2, line = 2)



par(mar = c(3, 6, 3, 2) + 0.1)
plot(test[[1]]$dds+100, test[[1]]$ref, type = "l", xlim = c(180, 1200), ylim = c(0, 0.0045), yaxt = "n", xlab = "", 
     cex.lab = 2, cex.axis = 2, ylab = "")
axis(2, at = seq(0, 0.0045, 0.001), labels = F)

polygon(test[[1]]$dds+100, test[[1]]$ref, col = "brown", border = NA)
polygon(test[[1]]$dds+100, test[[1]]$control, col= "brown", border = NA)

title(ylab = "Relative number", cex.lab = 2, line = 2)

polygon(seq(500-300, 1201-300), c(nocont, 0), col = mycol1, border = NA)
polygon(seq(500-300, 1201-300), c(control0, 0), col= "darkgreen")




par(mar = c(3, 6, 3, 2) + 0.1)
plot(test[[1]]$dds+100, test[[1]]$ref, type = "l", xlim = c(180, 1200), ylim = c(0, 0.0045), yaxt = "n", xlab = "", 
     cex.lab = 2, cex.axis = 2, ylab = "")
axis(2, at = seq(0, 0.0045, 0.001), labels = F)

polygon(test[[1]]$dds+100, test[[1]]$ref, col = "brown", border = NA)
polygon(test[[1]]$dds+100, test[[1]]$control, col= "brown", border = NA)

title(ylab = "Relative number", cex.lab = 2, line = 2)

polygon(seq(500-100, 1201-100), c(nocont, 0), col = mycol1, border = NA)
polygon(seq(500-100, 1201-100), c(control0, 0), col= "darkgreen")
