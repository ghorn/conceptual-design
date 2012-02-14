function loadFactor
clc

% constants
meanAerodynamicChordFt = 4;
S_m = 13.3780378; % m^2 == 144 ft^2
S_ft = 144;
g = 9.8;
W_lb = 5255;
m = 0.45359237*W_lb; % mass in kg
W_kg = m*g;
rho = 0.65295;  % SI
rho0 = 1.22419; % SI
a_mps = 316; % speed of sound in mps at 20,000 ft

% v_c and v_d from plackard computation
v_c_mph = 404.30876376735307; % true airspeed mph
v_d_mph = 464.95507833245597; % true airspeed mph
ve_c_mph = v_c_mph*sqrt(rho/rho0);
ve_d_mph = v_d_mph*sqrt(rho/rho0);
ve_c_knots = ve_c_mph*0.868976242;
ve_d_knots = ve_d_mph*0.868976242;
fprintf('ve_c_mph: %f\n',ve_c_mph);
fprintf('ve_d_mph: %f\n',ve_d_mph);

% independent variable
v_mph = linspace(1,1.3*ve_d_mph, 1000);
v_mps = v_mph*0.44704;
ve_mph = sqrt(rho/rho0)*v_mph;
ve_knots = 0.868976242*ve_mph;
mach = v_mps/a_mps;

% n from CL_max:
re = 5775*v_mph*meanAerodynamicChordFt; % for comparison only
re0 = 4.5e6;
n_CLmax = 0.5*rho*v_mps.^2.*clMax(mach, re0)*S_m/W_kg;

% when does n_CLmax == n_FAR
n_FAR = 2.1 + 24000/(W_lb + 10000);
fprintf('\nn_FAR: %f\n', n_FAR);
[~,k_m] = min(abs(n_CLmax-n_FAR));
fprintf('n_FAR == n_CLmax at ve: %f (mph)\n', ve_mph(k_m));

% gust load factors
a_mph = 707;
n_bs = gustLoadFactor(66, ve_knots,   W_lb, W_kg, S_m, S_ft, rho, v_mph/a_mph);
n_c  = gustLoadFactor(50, ve_c_knots, W_lb, W_kg, S_m, S_ft, rho, v_c_mph/a_mph);
n_d  = gustLoadFactor(25, ve_d_knots, W_lb, W_kg, S_m, S_ft, rho, v_d_mph/a_mph);
fprintf('\nn_c: %f\n',n_c);
fprintf('n_d: %f\n',n_d);

% stall speed:
[~,k_vs]=min(abs(n_CLmax - 1));
ve_s_mph = ve_mph(k_vs);
fprintf('\nve_s: %f mph (stall speed)\n', ve_s_mph);

% v_b:
% first solve for intersection of 1-g stall speed and n_b
[~, k] = min(abs(n_bs - n_CLmax));
ve_b0_mph = ve_mph(k);
fprintf('ve_b0: %f mph (intersection of n_b and n_CLmax)\n', ve_b0_mph);

% ve_s*sqrt(n_c)
ve_b1_mph = ve_s_mph*sqrt(n_c);
fprintf('ve_b1: %f mph (ve_s*sqrt(n_c))\n', ve_b1_mph);

% take the smaller speed:
ve_b_mph = min([ve_b0_mph, ve_b1_mph]);
fprintf('ve_b:  %f mph (min{ve_b0, ve_b1})\n', ve_b_mph);

% find n_b at v_b
[~,k_vb] = min(abs(ve_b_mph - ve_mph));
n_b = n_bs(k_vb);
fprintf('n_b:  %f\n', n_b);

figure(1)
subplot(311)
plot(ve_mph, clMax(mach, re0))
% plot(ve_mph, [clMax(mach, re0);clMax(mach, re)])
xlabel('v_e (mph)')
ylabel('CL_m_a_x')
title('CL_m_a_x vs. v_e')

subplot(312)
plot(ve_mph, n_CLmax)
xlabel('v_e (mph)')
ylabel('n_C_L_m_a_x')
title('n_C_L_m_a_x vs. v_e')

subplot(313)
hold off
plot(ve_mph(1:k_m), n_CLmax(1:k_m))
hold on
plot(ve_mph(k_m:end), n_FAR*ones(length(ve_mph(k_m:end))), 'r')
xlabel('v_e (mph)')
ylabel('n_m_a_n_e_u_v_e_r')
title('v-n (maneuver)')

figure(2)
hold off
plot(ve_mph(1:k_vb), n_CLmax(1:k_vb))
hold on
plot([ve_b_mph, ve_c_mph], [n_b, n_c],'r')
plot([ve_c_mph, ve_d_mph], [n_c, n_d],'g')
xlabel('v_e (mph)')
ylabel('n')
title('v-n (gust)')

figure(3)
% maneuver:
ve_m = [ve_mph(1:k_m), ve_mph(k_m:end)];
n_m = [n_CLmax(1:k_m), n_FAR*ones(1,length(ve_mph(k_m:end)))];
% gust:
ve_g = [ve_mph(1:k_vb), ve_b_mph, ve_c_mph, ve_d_mph];
n_g = [n_CLmax(1:k_vb), n_b, n_c, n_d];
hold off
plot(ve_m, n_m)
hold on
plot(ve_g, n_g,'r')
xlabel('v_e (mph)')
ylabel('n')
title('v-n')


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
