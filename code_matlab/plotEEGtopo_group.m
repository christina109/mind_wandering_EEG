function plotEEGtopo_group(datacell, tasks, states, chans2point, clim)
% Datacell: data averaged among participants by computeEEGtopo_group
% Tasks / States: [] - to plot the subset of the datacell
% Chans2point: the number of the channels to be marked on the topoplot
% Author: Christina Jin (christina.mik109@gmail.com)
% Last edit: Nov 21, 2018

    cd('c:\\topic_mind wandering\\3data\\')
    
    load('pars', 'chanlocs', 'scolors')
    
    if isempty(tasks)
        tasks = unique({datacell.task});
    end
    
    if isempty(states)
        states = unique({datacell.state});
    end
    
    % set color palette
    cmap = colormap(parula(16));

    for ti = 1:length(tasks)
        
        task = tasks{ti};
        
        for si = 1:length(states)
            
            state = states{si};
            scolor = scolors(strcmpi({scolors.state}, state)).color;
            
            data = datacell(strcmpi({datacell.task}, task) & strcmpi({datacell.state}, state)).data;

            % plot
            if si*ti == 1
                figure
            end
            subplot(length(tasks), length(states), (ti-1)*2+si)
            if ~isempty(clim)
                topoplot(data, chanlocs, 'style', 'map','emarker2', {chans2point, 'p','k',14,1}, 'hcolor', scolor, 'maplimits', clim, 'colormap', cmap)
            else
                topoplot(data, chanlocs, 'style', 'map','emarker2', {chans2point, 'p','k',14,1}, 'hcolor', scolor, 'colormap', cmap)
            end
            colorbar
            title([upper(task), ': ', upper(state)])         
                       
        end  % loop over states
    end  % loop over tasks
    
    suptitle(['Marker: ', upper(datacell(1).measure), '. Time window: ', num2str(datacell(1).timewin)])
    
end  % func