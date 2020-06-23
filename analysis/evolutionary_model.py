# evolutionary model
# this file produces the results in Figure 7 of the manuscript
# author: @lrdegeest

import matplotlib.pyplot as plt
plt.rcParams["font.family"] = "serif"

def run_sim(T):
    # population shares, assume equal 
    ## Low
    low_share = 0.5/2
    xL_D = [low_share]
    xL_C = [low_share]
    ## High
    high_share = 0.5/3
    xH_D = [high_share]
    xH_C1 = [high_share]
    xH_C2 = [high_share]

    # time step
    dt = 0.1

    # fitnesses
    ## Low
    F_L_D = [(xH_D[0]*piL_dd + xH_C1[0]*piL_dc1 + xH_C2[0]*piL_dc2) * dt]
    F_L_C = [(xH_D[0]*piL_cd + xH_C1[0]*piL_cc1 + xH_C2[0]*piL_cc2) * dt]

    ## High
    F_H_D = [(xL_D[0]*piH_dd + xL_C[0]*piH_dc) * dt]
    F_H_C1 = [(xL_D[0]*piH_c1d + xL_C[0]*piH_c1c) * dt]
    F_H_C2 = [(xL_D[0]*piH_c2d + xL_C[0]*piH_c2c) * dt]

    ## average fitness
    F = [(xL_D[0]*F_L_D[0] + xL_C[0]*F_L_C[0] + xH_D[0]*F_H_D[0] + xH_C1[0]*F_H_C1[0] + xH_C2[0]*F_H_C2[0]) * dt]

    # simulate
    for t in range(T):
        # fitnesses
        ## Low
        fLD_t = xH_D[t]*piL_dd + xH_C1[t]*piL_dc1 + xH_C2[t]*piL_dc2
        F_L_D.append(fLD_t*dt)
        fLC_t = xH_D[t]*piL_cd + xH_C1[t]*piL_cc1 + xH_C2[t]*piL_cc2
        F_L_C.append(fLC_t*dt)
        ## High
        fHD_t = xL_D[t]*piH_dd + xL_C[t]*piH_dc
        F_H_D.append(fHD_t)
        fHC1_t = xL_D[t]*piH_c1d + xL_C[t]*piH_c1c
        F_H_C1.append(fHC1_t)
        fHC2_t = xL_D[t]*piH_c2d + xL_C[t]*piH_c2c
        F_H_C2.append(fHC2_t)
        ## average
        f_t = xL_D[t]*fLD_t + xL_C[t]*fLC_t + xH_D[t]*fHD_t + xH_C1[t]*fHC1_t + xH_C2[t]*fHC2_t
        F.append(f_t*dt)
        # differential equations (evolution of population shares)
        ## Low
        xL_D.append(xL_D[t] + (xL_D[t] * (fLD_t - f_t)) * dt)
        xL_C.append(xL_C[t] + (xL_C[t] * (fLC_t - f_t)) * dt)
        ## High
        xH_D.append(xH_D[t] + (xH_D[t] * (fHD_t - f_t)) * dt)
        xH_C1.append(xH_C1[t] + (xH_C1[t] * (fHC1_t - f_t)) * dt)
        xH_C2.append(xH_C2[t] + (xH_C2[t] * (fHC2_t - f_t)) * dt)    
        
    # low shares
    low = [x + y for x, y in zip(xL_D, xL_C)]
    xL_D_share = [x/y for x, y in zip(xL_D, low)]
    xL_C_share = [x/y for x, y in zip(xL_C, low)]  
    
    # high shares
    high = [x + y + z for x, y, z in zip(xH_D, xH_C1, xH_C2)]
    xH_D_share = [x/y for x, y in zip(xH_D, high)]
    xH_C1_share = [x/y for x, y in zip(xH_C1, high)] 
    xH_C2_share = [x/y for x, y in zip(xH_C2, high)]
    
    # plot
    plt.plot(xL_D_share, 'b', label = "Low {0}: %5.2f" % round(xL_D_share[T],2))
    plt.plot(xL_C_share, 'b--', label = "Low {10}: %5.2f" % round(xL_C_share[T],2))
    plt.plot(xH_D_share, '-', color="darkorange", label = "High {0}: %5.2f" % round(xH_D_share[T],2))
    plt.plot(xH_C1_share, ':', color="darkorange", label = "High {10}: %5.2f" % round(xH_C1_share[T],2))
    plt.plot(xH_C2_share, '--', color="darkorange", label = "High {30}: %5.2f" % round(xH_C2_share[T],2))
    plt.legend(loc = 'best',frameon=False)
    plt.xlabel("Time")
    plt.ylabel("Share of strategy played\nwithin Low or High")
    plt.show()
    pass


# payoffs
def pi(e,x,y,mpcr=0.8):
    pi = (e - x) + mpcr*(x+y)
    return(pi)

# deterrence levels
deter_low = (pi(10,0,10) - pi(10,10,10)) + 1 
deter_high_d = pi(30,0,10) - pi(30,30,10) + 1
deter_high_c = pi(30,10,10) - pi(30,30,10) + 1


# Payoffs (need to update arguments for each simulation condition)
## Low c
piL_cd = pi(10,10,0)
piL_cc1 = pi(10,10,10)
piL_cc2 = pi(10,10,30)
## Low D 
piL_dd = pi(10,0,0)
piL_dc1 = pi(10,0,10)
piL_dc2 = pi(10,0,30)
## High c
piH_c1d = pi(30,10,0)
piH_c1c = pi(30,10,10)
## High C
piH_c2d = pi(30,30,0)
piH_c2c = pi(30,30,10)
## High D
piH_dd = pi(30,0,0)
piH_dc = pi(30,0,10)    


# run simulation
run_sim(200)