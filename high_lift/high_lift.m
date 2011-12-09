function high_lift()

clc
all.Cl_max0 = 1.68;
all.flap_chord_over_wing_chord = 0.25;
all.sweep_deg = 20;
all.Swf_over_Sref = 0.6;

cruise = all;
cruise.re = 7.6e6;
cruise.flap_angle_deg = 0;
cruise.mach = 0.55;
cruise.mach_correction_factor = 0.9/1.4;

fprintf('-----------------------------------------\n')
fprintf('----------------- cruise ----------------\n')
fprintf('-----------------------------------------\n')
run_high_lift(cruise);


landing_nf = all;
landing_nf.re = 3.1e6;
landing_nf.flap_angle_deg = 0;
landing_nf.mach = 0.106;
landing_nf.mach_correction_factor = 1.0;

fprintf('\n\n')
fprintf('-----------------------------------------\n')
fprintf('-------- no flaps takeoff/landing -------\n');
fprintf('-----------------------------------------\n')
run_high_lift(landing_nf);


takeoff = all;
takeoff.re = 3.1e6;
takeoff.flap_angle_deg = 30;
takeoff.mach = 0.106;
takeoff.mach_correction_factor = 1.0;

fprintf('\n\n')
fprintf('-----------------------------------------\n')
fprintf('---------- takeoff 30 deg flaps ---------\n');
fprintf('-----------------------------------------\n')
run_high_lift(takeoff);


landing = all;
landing.re = 3.1e6;
landing.flap_angle_deg = 40;
landing.mach = 0.106;
landing.mach_correction_factor = 1.0;

fprintf('\n\n')
fprintf('-----------------------------------------\n')
fprintf('---------- landing 40 deg flaps ---------\n');
fprintf('-----------------------------------------\n')
run_high_lift(landing);


function CL_max = run_high_lift(config)

CL_max = config.Cl_max0;
fprintf('initial Cl_max: %f\n\n', CL_max)

% reynolds correction
k_re = (config.re/9e6)^0.1;
CL_max = CL_max*k_re;
fprintf('reynolds number: %g\nreynolds correction factor: %f\nnew Cl_max: %f\n\n', config.re, k_re, CL_max)

% outer panel Cl_max to wing CL_max
k_op2w = 0.9;
CL_max = CL_max*k_op2w;
fprintf('outer panel --> wing CL_max correction: %.2f\nnew CL_max: %f\n\n', k_op2w, CL_max)

% FAR acceleration
k_far = 1.11;
CL_max = CL_max/k_far;
fprintf('FAR acceleration correction factor: 1 / %.3f\nnew CL_max: %f\n\n', k_far, CL_max)

% no slats or wing-mounted engines
fprintf('(no slats or wing-mounted engines)\n\n')

% flaps
fprintf('\t===== delta_Cl_max_flaps =====\n')
fprintf('\tflap chord over wing chord: %.3f\n', config.flap_chord_over_wing_chord);
k1 = config.flap_chord_over_wing_chord/0.25;
fprintf('\tk1: %.3f\n\n', k1)

fprintf('\tflap angle: %.3f degrees\n', config.flap_angle_deg);
k2 = config.flap_angle_deg/50;
fprintf('\tk2: %.3f\n\n', k2)

delta_Cl_max_ref = 1.9;
fprintf('\tdelta_Cl_max_ref: %.2f (from figure 8 at 14%% t/c)\n\n', delta_Cl_max_ref)

delta_Cl_max_flaps = k1*k2*delta_Cl_max_ref;
fprintf('\tdelta_Cl_max_flaps: %.3f ( == k1*k2*delta_Cl_max_ref)\n\n', delta_Cl_max_flaps)

sweep = config.sweep_deg*pi/180;
k_sweep = (1 - 0.08*cos(sweep)^2)*cos(sweep)^0.75;

fprintf('sweep: %.1f degrees\nk_sweep: %f\n', config.sweep_deg, k_sweep)

delta_CL_max_flaps = config.Swf_over_Sref*delta_Cl_max_flaps*k_sweep;
fprintf('delta_CL_max_flaps: %f ( == Swf/Sref*delta_Cl_max_flaps*k_sweep)\n', delta_CL_max_flaps)

CL_max = CL_max + delta_CL_max_flaps;
fprintf('new CL_max (after flaps): %f\n\n', CL_max)

% mach correction
M_prime = config.mach*cosd(config.sweep_deg)/cosd(24.5);
CL_max = CL_max*config.mach_correction_factor;
fprintf('M_prime: %f\n', M_prime);
fprintf('Mach correction factor: %f\n', config.mach_correction_factor);
fprintf('final CL_max: %f\n', CL_max);





