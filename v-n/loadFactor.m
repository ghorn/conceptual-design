function loadFactor
clc
meanAerodynamicChordFt = 4;
S_m = 13.3780378; % m^2 == 144 ft^2
S_ft = 144;
m = 2721.55422; % kg == 6000 lb
g = 9.8;
W_kg = m*g;
W_lb = 6000;
rho = 0.65295;  % SI
rho0 = 1.22419; % SI
a_mps = 316; % speed of sound in mps at 20,000 ft

v_mps = linspace(1,200, 1000);
v_mph = v_mps/0.44704;
v_equiv_mph = sqrt(rho/rho0)*v_mph;
v_equiv_knots = 0.868976242*v_equiv_mph;
mach = v_mps/a_mps;

v_c_mph = 404.30876376735307; % true airspeed mph
v_d_mph = 464.95507833245597; % true airspeed mph
ve_c_mph = v_c_mph*sqrt(rho/rho0);
ve_d_mph = v_d_mph*sqrt(rho/rho0);
ve_c_knots = ve_c_mph*0.868976242;
ve_d_knots = ve_d_mph*0.868976242;

a_mph = 707;
n_cd = gustLoadFactor([50, 25], [ve_c_knots, ve_d_knots], W_lb, W_kg, S_m, S_ft, rho, [v_c_mph, v_d_mph]/a_mph);
n_c = n_cd(1);
n_d = n_cd(2);

re = 5775*v_mph*meanAerodynamicChordFt; % for comparison only
re0 = 4.5e6;
n_CLmax = 0.5*rho*v_mps.^2.*clMax(mach, re0)*S_m/W_kg;

n_FAR = 2.1 + 24000/(W_lb + 10000);


subplot(311)
plot(v_equiv_mph, [n_CLmax; n_FAR*ones(1,length(v_mph))])
hold on
plot(ve_c_mph, n_c,'ro')
plot(ve_d_mph, n_d,'ro')
xlabel('v_e_q_u_i_v (mph)')
ylabel('n maneuver')
title('v-n diagram at 20,000 ft')

subplot(312)
plot(v_equiv_mph, [clMax(mach, re0);clMax(mach, re)])
xlabel('v_e_q_u_i_v (mph)')
ylabel('CL_m_a_x')
title('CL_m_a_x vs equivilant airspeed')

subplot(313)
plot(v_equiv_mph, gustLoadFactor(50, v_equiv_knots, W_lb, W_kg, S_m, S_ft, rho, mach))
xlabel('v_e_q_u_i_v (mph)')
ylabel('n (gust load factor)')
title('gust load factor')



function n = gustLoadFactor(ue_fps, ve_knots, W_lb, W_kg, S_m, S_ft, rho, mach)

beta = sqrt(1 - mach.^2);
AR = 9;
delta = 0;
kappa = 0.97;
g = 9.8;
a = 2*pi*AR./(2 + sqrt(AR^2*beta.^2/kappa^2.*(1+tan(delta)^2./beta.^2)+4));% dCldAlpha

c_meanGeometric = sqrt(5.5*(5.5/2.2)); % mean geometric chord in meters;
mu = 2*(W_kg/S_m)./(rho*c_meanGeometric*a*g);
kg = 0.88*mu./(5.3+mu);

n = 1 + kg.*a.*ue_fps.*ve_knots./(498*(W_lb/S_ft));
