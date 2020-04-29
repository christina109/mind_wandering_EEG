function n = count_trials(sub, triggers, rmRej)

data = select_trials(sub, triggers, rmRej);
n    = size(data,3);

end % func