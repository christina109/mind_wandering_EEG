function compareEEGwave_group(waveStruc, timewin, states, tasks)
% WaveStruc: the wave data structure obtained througth computeEEGwave_group(averageOn = 0)
% Timewin: the time window where data points are averaged
% States: states used to compared, [] - to use default states
% Tasks: subset of the tasks, [] - to use all

    load('pars.mat', 'times') 

    % default setting
    if isempty(states)
        states = unique({waveStruc(:).state});
    end
    if isempty(tasks)
        tasks = unique({waveStruc(:).task});
    end

    timeidx = dsearchn(times', timewin');
    
    for ti = 1:length(tasks)
        task = tasks{ti};
        
        % get data: nPnt x nSub x nState
        for si = 1:length(states)
            state = states{si};
            temp = waveStruc(strcmpi({waveStruc.task}, task) & strcmpi({waveStruc.state}, state)).data;
            if si == 1
                data = zeros([size(temp), length(states)]);
            end
            data(:,:,si) = temp;    
        end
        
        % average in the specified timewin
        data = mean(data(timeidx(1):timeidx(2), :,:), 1);
        
        % t-test
        if length(states) == 2
            x = data(:,:,1);
            y = data(:,:,2);
            [h, p, ~, stats] = ttest(x, y);
            d = cohenD(x, y);
            disp(['Comparison made between ', upper(states{1}), ' and ', upper(states{2}), ' in task ', upper(task), ' is ', ifelse(h, 'SIGNIFICANT', 'N.S.'), '.'])
            disp([' t(', num2str(stats.df), ') = ', num2str(stats.tstat), ', p = ', num2str(p), ', d = ', num2str(d)])
        else 
            error('Unfishied code if condition number is larger than 2!')
        end
    end
        

end  % func