function data = get_data(sub, task, state, trialIdx)
% get data to plot based on the given task_state combination or the trial index
% input: trialIdx should be a 2-column-matrix: ncase x nsession
% return: a 3-d matrix raw data with size of nchan x npnt x ntrial
    
    cd('c:\\topic_mind wandering\\3data')
    
    % default pars:
    if nargin <4
       trialIdx = {};
    end

    if ~isempty(task)
         load('pars', 'tasks', 'states', 'triggersets')
        for si = 1:length(states)
            if strcmp(states{si}, state)
                break
            else
                if si == 2
                    error('Invalid state!')
                end
            end
        end
        
        for ti = 1:length(tasks)
            if strcmp(tasks{ti}, task)
                break
            else
                if ti == 2
                    error('Invalid task!')
                end
            end
        end
    end
    
    if iscell(trialIdx)  % load data by task_state combi
        
        data = select_trials(sub, triggersets{ti}{si});
        
    else  % load data by trialIdx (x task_state)
        
        if isempty(task)          
            triggerbase = 10:10:60;
            triggermat = repmat(triggerbase, 7, 1) + repmat(0:6, 6, 1)';
            [data, rawIdx, ~] = select_trials(sub, reshape(triggermat, 1, numel(triggermat)));
        else
            [data, rawIdx, ~] = select_trials(sub, triggersets{ti}{si});
        end
        
        trial2select = ismember(rawIdx, trialIdx, 'rows');
        data = data(:,:,trial2select);
        
    end 

end  % func