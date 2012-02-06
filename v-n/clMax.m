function CL_max = clMax(mach, re)

config.Cl_max0 = 1.68;
config.flap_chord_over_wing_chord = 0.25;
config.sweep_deg = 0;
config.Swf_over_Sref = 0.6;

config.re = re;
config.flap_angle_deg = 0;
config.mach = mach;


CL_max = config.Cl_max0;
% fprintf('initial Cl_max: %f\n\n', CL_max)

% reynolds correction
k_re = (config.re/9e6).^0.1;
CL_max = CL_max*k_re;

% outer panel Cl_max to wing CL_max
k_op2w = 0.9;
CL_max = CL_max*k_op2w;

% FAR acceleration
k_far = 1.11;
CL_max = CL_max*k_far;

% flaps
k1 = config.flap_chord_over_wing_chord/0.25;
k2 = config.flap_angle_deg/50;
delta_Cl_max_ref = 1.9;
delta_Cl_max_flaps = k1*k2*delta_Cl_max_ref;
sweep = config.sweep_deg*pi/180;
k_sweep = (1 - 0.08*cos(sweep)^2)*cos(sweep)^0.75;
delta_CL_max_flaps = config.Swf_over_Sref*delta_Cl_max_flaps*k_sweep;
CL_max = CL_max + delta_CL_max_flaps;

% mach correction
M_prime = config.mach*cosd(config.sweep_deg)/cosd(24.5);
k_mach = (M_prime > 0.26).*(1.4 + (M_prime - 0.26)*(0.9-1.4)/(0.55-0.26))/1.4 + (M_prime <= 0.26);
CL_max = CL_max.*k_mach;
% fprintf('final CL_max: %f\n', CL_max);
