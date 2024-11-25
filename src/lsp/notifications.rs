use crate::{
    editor::{Action, Actions},
    lsp::msg::Notification,
};
use lsp_types::{
    notification::{Notification as _, Progress},
    NumberOrString, ProgressParams,
};
use std::collections::HashMap;
use tracing::{error, warn};

pub fn try_parse_notification(
    n: Notification,
    tokens: &mut HashMap<NumberOrString, String>,
) -> Option<Actions> {
    if n.method == Progress::METHOD {
        match n.extract::<Progress>() {
            Ok(params) => handle_progress(params, tokens),
            Err(e) => {
                error!("malformed progress notification: {e}");
                None
            }
        }
    } else {
        warn!(
            "unknown notification from LSP: {}",
            serde_json::to_string(&n).unwrap()
        );
        None
    }
}

fn handle_progress(
    params: ProgressParams,
    tokens: &mut HashMap<NumberOrString, String>,
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

    match params.value {
        WorkDone(Begin(WorkDoneProgressBegin {
            title,
            message,
            percentage,
            ..
        })) => {
            let actions = actions(&title, message, percentage);
            tokens.insert(params.token, title);

            actions
        }

        WorkDone(Report(WorkDoneProgressReport {
            message,
            percentage,
            ..
        })) => {
            let title: &str = tokens.get(&params.token).map_or("", |s| s);
            actions(title, message, percentage)
        }

        WorkDone(End(WorkDoneProgressEnd { .. })) => {
            tokens.remove(&params.token);

            // Clear the status message when progress is done
            Some(Actions::Single(Action::SetStatusMessage {
                message: "".to_owned(),
            }))
        }
    }
}
