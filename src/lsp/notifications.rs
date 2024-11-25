use crate::{
    editor::{Action, Actions},
    lsp::msg::Notification,
};
use lsp_types::{notification::Progress, NumberOrString, ProgressParams};
use std::collections::HashMap;

pub fn try_parse_notification(
    n: Notification,
    tokens: &mut HashMap<String, String>,
) -> Option<Actions> {
    if let Ok(params) = n.extract::<Progress>() {
        handle_progress(params, tokens)
    } else {
        None
    }
}

fn handle_progress(
    params: ProgressParams,
    tokens: &mut HashMap<String, String>,
) -> Option<Actions> {
    use lsp_types::{
        ProgressParamsValue, WorkDoneProgress, WorkDoneProgressBegin, WorkDoneProgressEnd,
        WorkDoneProgressReport,
    };
    use ProgressParamsValue::*;
    use WorkDoneProgress::*;

    let actions = |title: &str, message: Option<String>, perc: Option<u32>| {
        let message = message.unwrap_or_default();
        let message = if let Some(perc) = perc {
            format!("{title}: {message} ({perc}/100)")
        } else {
            format!("{title}: {message}")
        };

        Some(Actions::Single(Action::SetStatusMessage { message }))
    };

    let token = match params.token {
        NumberOrString::String(token) => token,
        NumberOrString::Number(i) => i.to_string(),
    };

    match params.value {
        WorkDone(Begin(WorkDoneProgressBegin {
            title,
            message,
            percentage,
            ..
        })) => {
            let actions = actions(&title, message, percentage);
            tokens.insert(token, title);

            actions
        }

        WorkDone(Report(WorkDoneProgressReport {
            message,
            percentage,
            ..
        })) => {
            let title: &str = tokens.get(&token).map_or("", |s| s);
            actions(title, message, percentage)
        }

        WorkDone(End(WorkDoneProgressEnd { message })) => {
            let title: &str = tokens.get(&token).map_or("", |s| s);
            actions(title, message, None)
        }
    }
}
