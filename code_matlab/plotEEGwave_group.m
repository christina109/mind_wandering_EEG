function plotEEGwave_group(datacell, tasks, states, timelim)
% Datacell: data averaged among participants by computeEEGwave_group
% Tasks / States: [] - to plot the subset of the datacell
% Author: Christina Jin (christina.mik109@gmail.com)
% Last edit: Nov 26, 2018

    cd('c:\\topic_mind wandering\\3data\\')
    
    load('pars', 'times', 'scolors')
    
    if isempty(tasks)
        tasks = unique({datacell.task});
    end
    
    if isempty(states)
        states = unique({datacell.state});
    end
    
    for ti = 1:length(tasks)
        task = tasks{ti};
        for si = 1:length(states)
            state = states{si};
            scolor = scolors(strcmpi({scolors.state}, state)).color;
            data = datacell(strcmpi({datacell.task}, task) & strcmpi({datacell.state}, state)).data;
            se = datacell(strcmpi({datacell.task}, task) & strcmpi({datacell.state}, state)).sewithin;

            % plot
            if si*ti == 1
                figure
            end
            subplot(length(tasks), 1, ti)
            shadedErrorBar(times, data,  se, {'Color', scolor, 'lineWidth', 2}, 1);
            if si < length(states)
                hold on
            else
                gap = ifelse(max(timelim > 500), 400, 200);             
                set(gca, 'xtick', 0:gap:1200);
                set(gca, 'fontsize', 38)
                xlim(timelim)
                title([upper(task), ' chans: ', num2str(datacell(1).chans)], 'FontSize', 12)
                xlabel('Time [ms]', 'FontSize', 30)
                ylabel('Amplitude [\muV]', 'FontSize', 30)
                hold off
            end
        end
    end

end  % func
